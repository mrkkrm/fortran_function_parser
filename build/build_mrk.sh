
## parser test

rm -f *.mod *.o *.x

flags="-c -g -O0 -check bounds -traceback" # "-ffpe-trap=invalid,zero,overflow"

FC='ifort -diag-disable=10448'

${FC} ${flags} ../src/error_module.f90
${FC} ${flags} ../src/function_parser.F90
${FC} ${flags} ../test/tests_mrk.f90
${FC} function_parser.o error_module.o tests_mrk.o -o tests_mrk.x

./tests_mrk.x
