#!/bin/bash

LIBDIR=/nobackup/iankim/pythia-pgs/libraries/PGS4/lib/
PYTHIALIB=/nobackup/iankim/pythia-pgs/libraries/pylib/lib/
PDFLIB=/nobackup/iankim/pythia-pgs/libraries/lhapdf/lib/

gfortran -c hepevt2stdhep.f
gfortran -o hepevt2stdhep.iw hepevt2stdhep.o $LIBDIR/libpgslib.a $LIBDIR/libexthep.a  $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PYTHIALIB/libpythiaext.a   $LIBDIR/libtauola.a



gfortran -c hep2lhe.f
gfortran -c ME2pythia.f
gfortran -c getjet.f
gfortran -c ktclusdble.f
gfortran -c pgs_ranmar.f
gfortran -c stdhep_print.f
gfortran -o hep2lhe.iw hep2lhe.o ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $LIBDIR/libtauola.a $PYTHIALIB/libpythiaext.a $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PDFLIB/libLHAPDF.a

gfortran -o stdhep_print.iw stdhep_print.o ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $LIBDIR/libtauola.a $PYTHIALIB/libpythiaext.a $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PDFLIB/libLHAPDF.a



#gfortran -o pythia.iw pythia.f ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $LIBDIR/libtauola.a $PYTHIALIB/libpythiaext.a $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PDFLIB/libLHAPDF.a


