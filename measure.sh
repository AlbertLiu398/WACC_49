./wacc-compiler 1.wacc
./wacc-compiler 2.wacc -o

declare num_iterations=100

start=$(date +%s%N)
for ((i = 1; i <= num_iterations; i++))
do
    aarch64-linux-gnu-gcc -o EXEName1 -z noexecstack -march=armv8-a 1.s
    qemu-aarch64 -L /usr/aarch64-linux-gnu/ EXEName1
done
end=$(date +%s%N)
runtime=$((($end - $start) / 1000000))
file_size1=$(stat -c "%s" "1.s")

start=$(date +%s%N)
for ((i = 1; i <= num_iterations; i++))
do
    aarch64-linux-gnu-gcc -o EXEName2 -z noexecstack -march=armv8-a 2.s
    qemu-aarch64 -L /usr/aarch64-linux-gnu/ EXEName2
    
done
end=$(date +%s%N)
runtime_optimised=$((($end - $start) / 1000000))
file_size2=$(stat -c "%s" "2.s")

echo "--------Execution time: $runtime miliseconds-----------"
echo "--------Optimised Execution time: $runtime_optimised miliseconds--------"

echo "--------File size 1: $file_size1 bytes--------"
echo "--------File size 2 (optimised): $file_size2 bytes--------"

