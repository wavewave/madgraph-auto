#!/bin/bash

LIBDIR=../../mc/pythia-pgs/libraries/PGS4/lib/
PYTHIALIB=../../mc/pythia-pgs/libraries/pylib/lib/
PDFLIB=../../mc/pythia-pgs/libraries/lhapdf/lib/

gfortran -c hepevt2stdhep.f
gfortran -o hepevt2stdhep.iw hepevt2stdhep.o $LIBDIR/libpgslib.a $LIBDIR/libtauola.a $LIBDIR/libstdhep.a $LIBDIR/libFmcfio.a 

#$LIBDIR/libexthep.a
