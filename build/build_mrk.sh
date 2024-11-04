
## parser test

rm -f *.mod *.o *.x

FC='ifort -diag-disable=10448'
flags="-c -g -O0 -check bounds -traceback" # "-ffpe-trap=invalid,zero,overflow"

#FC='gfortran'
#flags="-c -g -O0" 

${FC} ${flags} ../test/selection_mod.F90
${FC} ${flags} ../src/error_module.f90
${FC} ${flags} ../src/function_parser.F90
${FC} ${flags} ../test/tests_mrk.f90
${FC} ${flags} ../test/readwrite_mod.F90

${FC} selection_mod.o function_parser.o error_module.o tests_mrk.o -o tests_mrk.x

${FC} readwrite_mod.o -o rw.x

#./tests_mrk.x

./rw.x
