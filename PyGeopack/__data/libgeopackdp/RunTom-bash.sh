#/bin/bash
if [[ $LD_LIBRARY_PATH == *"$(pwd)"* ]]; then
	echo "No need to update LD_LIBRARY_PATH"
else
	echo "Updating LD_LIBRARY_PATH"
	export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$(pwd)"
fi

./test
