echo.off

REM Compiled with: ghc -threaded -O2 --make "solve.hs"
REM Execute in parallel: solve +RTS -N6 -s -RTS

.\solve

pause