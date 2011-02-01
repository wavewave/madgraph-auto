#!/bin/bash

LIBDIR=../../mc/pythia-pgs/libraries/PGS4/lib/
PYTHIALIB=../../mc/pythia-pgs/libraries/pylib/lib/
PDFLIB=../../mc/pythia-pgs/libraries/lhapdf/lib/

#gfortran -c hepevt2stdhep.f
#gfortran -o hepevt2stdhep.iw hepevt2stdhep.o $LIBDIR/libpgslib.a $LIBDIR/libexthep.a  $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PYTHIALIB/libpythiaext.a   $LIBDIR/libtauola.a



gfortran -c hep2lhe.f
gfortran -c ME2pythia.f
gfortran -c getjet.f
gfortran -c ktclusdble.f
gfortran -c pgs_ranmar.f
gfortran -o hep2lhe.iw hep2lhe.o ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $LIBDIR/libtauola.a $PYTHIALIB/libpythiaext.a $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PDFLIB/libLHAPDF.a

#gfortran -o pythia.iw pythia.f ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o $LIBDIR/libtauola.a $PYTHIALIB/libpythiaext.a $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a $PDFLIB/libLHAPDF.a


