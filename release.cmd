
del /F /S /Q build

stack clean
stack build --copy-bins --local-bin-path=build/bin/windows
tar zcf build\bin-windows.tar.gz -C build bin
