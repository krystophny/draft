&physicslist
 Igeometry   =         3
 Istellsym   =         1
 Lfreebound  =         0
 phiedge     =   2.000000000000000E+00
 curtor      =   1.038123580200000E-09
 curpol      =   0.000000000000000E+00
 gamma       =   0.000000000000000E+00
 Nfp         =         5
 Nvol        =         2
 Mpol        =         3
 Ntor        =         3
 Lrad        =                     8  8
 tflux       =   0.5e0 1.000000000000000E+00
 pflux       =   0.5e0 -2.040878894181875E-01
 helicity    =   1e-3 1.559429589793997E-03
 pscale      =   0.000000000000000E+00
 Ladiabatic  =         0
 pressure    =   0.000000000000000E+00  0.0e0
 adiabatic   =   1.000000000000000E+00  0.000000000000000E+00
 mu          =   0.000000000000000E+00
 Lconstraint =         1
 pl          =                       0                      0                      0
 ql          =                       0                      0                      0
 pr          =                       0                      0                      0
 qr          =                       0                      0                      0
 iota        =   0.000000000000000E+00  1e-1 2.809417939338480E-01  3.050000000000000E-01
 lp          =                       0                      0                      0
 lq          =                       0                      0                      0
 rp          =                       0                      0                      0
 rq          =                       0                      0                      0
 oita        =   0.000000000000000E+00  1e-1 2.809417939338480E-01  3.050000000000000E-01
 mupftol     =   1.000000000000000E-12
 mupfits     =       128
 Rac         =   1.0
 Zas         =   0.0
  ! Mean major radius:
  RBC(0,0) =   1.0E+00     ZBS(0,0) =   0.0000E+00

  ! Helicity of the axis:
  RBC(1,0) =   0     ZBS(1,0) =   0

  ! axisymmetric circular or elliptical cross-section
  RBC(0,1) =   1.0E-01     ZBS(0,1) =   1.0E-01

  ! Rotating elongation
  RBC(1,1) = 0.05    ZBS(1,1) = -0.05
/
&numericlist
 Linitialize =         1
 Ndiscrete   =         2
 Nquad       =        -1
 iMpol       =        -4
 iNtor       =        -4
 Lsparse     =         0
 Lsvdiota    =         0
 imethod     =         3
 iorder      =         2
 iprecon     =         1
 iotatol     =  -1.000000000000000E+00
/
&locallist
 LBeltrami   =         4
 Linitgues   =         1
/
&globallist
 Lfindzero   =         2
 escale      =   0.000000000000000E+00
 pcondense   =   4.000000000000000E+00
 forcetol    =   1.000000000000000E-12
 c05xtol     =   1.000000000000000E-12
 c05factor   =   1.000000000000000E-04
 LreadGF     =         F
 opsilon     =   1.000000000000000E+00
 epsilon     =   1.000000000000000E+00
 upsilon     =   1.000000000000000E+00
/
&diagnosticslist
 odetol      =   1.000000000000000E-07
 absreq      =   1.000000000000000E-08
 relreq      =   1.000000000000000E-08
 absacc      =   1.000000000000000E-04
 epsr        =   1.000000000000000E-08
 nPpts       =        20
 nPtrj       =        5
 LHevalues   =         F
 LHevectors  =         F
/
&screenlist
 Wpp00aa = T
/
