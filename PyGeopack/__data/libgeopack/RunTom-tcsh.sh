#/bin/tcsh
set wd = `pwd`
if (! $?LD_LIBRARY_PATH) then
	echo "setting LD_LIBRARY_PATH"
	setenv LD_LIBRARY_PATH $wd
else
	echo "updating $LD_LIBRARY_PATH"
	setenv LD_LIBRARY_PATH {$LD_LIBRARY_PATH}:$wd
endif
./test

