
Data.Avc
========

Ascii Vectors (AVC) are used on the Advantest 93k platform to represent test
patterns. This library reads and writes a simple subset of AVC. It only
supports `FORMAT` and `R` and representing pin states with a single character.
Use it if your file looks like:

    FORMAT pin1 pin2 ;
    R1 0H;
    R10 1L;

There's an included example `SelectSigs` which takes an AVC and removes all
except for your selected signals. For example:

    SelectSigs test.avc.gz jtg_trst_n jtg_tck jtg_tms jtg_tdi jtg_tdo

    FORMAT jtg_trst_n jtg_tck jtg_tms jtg_tdi jtg_tdo ;
    R1 tset1 0100Z ; 0.000 V:1 C:1
    R2 tset1 0100Z ; 20.000 V:2 C:2
    R1 tset1 0100Z ; 60.000 V:3 C:4
    R1 tset1 0100Z ; 80.000 V:4 C:5
    R96 tset1 0100Z ; 100.000 V:5 C:6
    ...

