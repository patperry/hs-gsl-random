#!/usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.PackageDescription
> import Distribution.Version
> 
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Program
> import Distribution.Verbosity
> 
> import Data.List (findIndices)
> 
> 
> main = defaultMainWithHooks simpleUserHooks {
>   hookedPrograms = [gslconfigProgram],
> 
>   confHook = \pkg flags -> do
>     lbi <- confHook simpleUserHooks pkg flags
>     bi <- gslBuildInfo lbi
>     
>     return lbi {
>       localPkgDescr = updatePackageDescription
>                         (Just bi, []) (localPkgDescr lbi)
>     } 
> }
> 
> gslconfigProgram = (simpleProgram "gsl-config") {
>     programFindLocation = \verbosity ->
>       findProgramLocation verbosity "gsl-config"
>   }
> 
> gslBuildInfo :: LocalBuildInfo -> IO BuildInfo
> gslBuildInfo lbi = do
>   (gslconfigProg, _) <- requireProgram verbosity
>                          gslconfigProgram (withPrograms lbi)
>   let gslconfig = rawSystemProgramStdout verbosity gslconfigProg
> 
>   cflags <- words `fmap` gslconfig ["--cflags"]
>   libs <- words `fmap` gslconfig ["--libs"]
> 
>   return emptyBuildInfo {
>       frameworks    =  [ libs !! (i+1)
>                        | i <- findIndices (== "-framework") libs
>                        , i + 1 < length libs ]
>     , extraLibs     = flag "-l" libs
>     , extraLibDirs  = flag "-L" libs
>     , includeDirs   = flag "-I" cflags
>   }
>   where
>     verbosity = normal -- honestly, this is a hack
>     flag f ws = 
>       let l = length f in [ drop l w | w <- ws, take l w == f ]
>     
> 