
echo "Running uncorrelated"
for /l %%x in (1, 1, 10) do (
   Rscript.exe Simulate_SSL_Rockova.r %%x 100 "FALSE"
   Rscript.exe Simulate_SCAD.r %%x 100 "FALSE"
   REM Rscript.exe Simulate_NonLocal_Rossell.r %%x 100 "FALSE"
)

echo "Running correlated"
for /l %%x in (1, 1, 10) do (
   Rscript.exe Simulate_SSL_Rockova.r %%x 100 "TRUE"
   Rscript.exe Simulate_SCAD.r %%x 100 "TRUE"
   REM Rscript.exe Simulate_NonLocal_Rossell.r %%x 100 "TRUE"
)

