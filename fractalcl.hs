import Control.Parallel.OpenCL
import Control.Monad (liftM)
import Foreign(castPtr, nullPtr, sizeOf)
import Foreign.C.Types(CFloat, CInt)
import Foreign.Marshal.Array(newArray, peekArray, mallocArray)
import Control.Exception (catch)
import System.Exit (exitFailure)
import Prelude hiding (catch)
import Foreign.Storable
import Foreign.Ptr (plusPtr, castPtr, Ptr)
import Graphics.Gloss hiding (dim)
import Foreign.ForeignPtr (newForeignPtr_, ForeignPtr)
import Data.Word (Word8)

data Float2 = Float2 CFloat CFloat deriving (Show, Eq)
data Int2 = Int2 CInt CInt deriving (Show, Eq)

instance Storable Float2 where
    sizeOf _ = 2 * sizeOf (undefined :: CFloat)
    alignment _ = alignment (undefined :: CFloat)
    peek = undefined
    poke ptr (Float2 x y) = do
        let ptr' = castPtr ptr
        poke ptr' x
        poke (ptr' `plusPtr` sizeOf (undefined :: CFloat)) y

instance Storable Int2 where
    sizeOf _ = 2 * sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    peek = undefined
    poke ptr (Int2 x y) = do
        let ptr' = castPtr ptr
        poke ptr' x
        poke (ptr' `plusPtr` sizeOf (undefined :: CInt)) y

programSource :: String
--programSource = "__kernel void duparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 2*in[id];\n}"
programSource = unlines
    [ "__kernel void fractal("
    , "int2 dimensions,"
    , "float2 start,"
    , "float2 step,"
    , "uint depth,"
    , "__global char* out) {"
    , "    uint index = get_global_id(0);"
    , "    uint x = index % dimensions.x;"
    , "    uint y = index / dimensions.x;"
    , "    //uint y = get_global_id(1);"
    , "    //uint index = x + dimensions.x * y;"
    , "    float2 c = (float2)(start.x + x*step.x, start.y + y*step.y);"
    , "    float2 point = (float2)(0.0f, 0.0f);"
    , "    float count = 0;"
    , "    for (int i = 0; i < depth; ++i) {"
    , "        if (dot(point, point) < 2) {"
    , "            count += 1;"
    , "            point = (float2)(point.x*point.x - point.y*point.y, 2*point.x*point.y) + c;"
    , "        }"
    , "    }"
    , "    count /= depth;"
    , "    count = sqrt(sqrt(count));"
    , "    char brightness = count * 255;"
    , "    out[index*4]   = brightness;"
    , "    out[index*4+1] = brightness;"
    , "    out[index*4+2] = brightness;"
    , "    out[index*4+3] = 255;"
    , "}"
    ]

main :: IO ()
main = do
    platform <- head `liftM` clGetPlatformIDs
    dev      <- head `liftM` clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
    context  <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
    q        <- clCreateCommandQueue context dev []

    program <- clCreateProgramWithSource context programSource
    catch (clBuildProgram program [dev] "")
          (\CL_BUILD_PROGRAM_FAILURE -> clGetProgramBuildLog program dev >>= putStrLn >> exitFailure)
    kernel <- clCreateKernel program "fractal"

    let dim = 400
        dim' = fromIntegral dim
        vecsize = dim * dim * sizeOf (undefined :: CFloat)
    mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecsize, nullPtr)

    mapM_ (\(i, set) -> catch set (\CL_INVALID_ARG_SIZE -> putStrLn ("Failed setting arg: " ++ show i))) $ zip [0..]
        [ clSetKernelArgSto kernel 0 (Int2 dim' dim')
        , clSetKernelArgSto kernel 1 (Float2 (-2) (-2))
        , clSetKernelArgSto kernel 2 (Float2 0.01 0.01)
        , clSetKernelArgSto kernel 3 (1000 :: CInt)
        , clSetKernelArgSto kernel 4 mem_out
        ]

    eventExec <- clEnqueueNDRangeKernel q kernel [dim * dim] [1] []

    output <- mallocArray (dim * dim * 4) :: IO (Ptr Word8)
    print =<< take 12 `liftM` peekArray vecsize output
    eventRead <- clEnqueueReadBuffer q mem_out True 0 vecsize (castPtr output) [eventExec]
    foutput <- newForeignPtr_ output :: IO (ForeignPtr Word8)

    let picture = bitmapOfForeignPtr dim dim foutput True
    animate (InWindow "Fractal" (dim, dim) (10, 10)) (makeColor 0 0 0 0) (const picture)
