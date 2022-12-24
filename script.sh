#!/bin/bash

echo -e 'Run ./script.sh and input your data\nIf you choose 5.1 algo, after filename just press enter 2 times'
read var1 
read var2 
read var3 
read var4
echo -e "$var1\n$var2\n$var3\n$var4" > input_from_file
Rscript script.R 
rm input_from_file
