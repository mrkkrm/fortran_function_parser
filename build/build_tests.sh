
## parser test

rm -f *.mod *.o *.x

flags="-c -g -O0" # "-ffpe-trap=invalid,zero,overflow"

FC='gfortran'

${FC} ${flags} ../src/error_module.f90
${FC} ${flags} ../src/function_parser.F90
${FC} ${flags} ../test/tests.f90
${FC} function_parser.o error_module.o tests.o -o tests.x

./tests.x
