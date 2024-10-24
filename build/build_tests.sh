
## parser test

rm -f *.mod *.o *.x

flags="-c -g -O0 -check bounds -traceback" # "-ffpe-trap=invalid,zero,overflow"

FC='ifort -diag-disable=10448'

${FC} ${flags} ../src/error_module.f90
${FC} ${flags} ../src/function_parser.F90
${FC} ${flags} ../test/tests.f90
${FC} function_parser.o error_module.o tests.o -o tests.x

./tests.x
