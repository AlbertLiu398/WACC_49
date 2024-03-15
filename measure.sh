./wacc-compiler 1.wacc
./wacc-compiler 2.wacc -o

declare num_iterations=100
declare convert_to_mili=1000000

#Unoptimised
start=$(date +%s%N)
for ((i = 1; i <= num_iterations; i++))
do
    aarch64-linux-gnu-gcc -o EXEName1 -z noexecstack -march=armv8-a 1.s
    qemu-aarch64 -L /usr/aarch64-linux-gnu/ EXEName1
done
end=$(date +%s%N)
runtime=$((($end - $start) / convert_to_mili))
line_count=$(wc -l < "1.s")


#Optimised
start=$(date +%s%N)
for ((i = 1; i <= num_iterations; i++))
do
    aarch64-linux-gnu-gcc -o EXEName2 -z noexecstack -march=armv8-a 2.s
    qemu-aarch64 -L /usr/aarch64-linux-gnu/ EXEName2
    
done
end=$(date +%s%N)
runtime_optimised=$((($end - $start) / convert_to_mili))
line_count_optimised=$(wc -l < "2.s")

echo "--------Total Execution time: $runtime miliseconds-----------"
echo "--------Optimised Total Execution time: $runtime_optimised miliseconds--------"

echo "--------Line count: $line_count--------"
echo "--------Line count (optimised): $line_count_optimised--------"


