!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE magfie(x,bmod,sqrtg,bder,hcovar,hctrvr,hcurl)
!
! Computes magnetic field module in units of the magnetic code  - bmod,
! square root of determinant of the metric tensor               - sqrtg,
! derivatives of the logarythm of the magnetic field module
! over coordinates                                              - bder,
! covariant componets of the unit vector of the magnetic
! field direction                                               - hcovar,
! contravariant components of this vector                       - hctrvr,
! contravariant component of the curl of this vector            - hcurl
! Now: Order of coordinates is x(1) = s, x(2) = phi, x(3) = theta
!      (Boozer coordinates from neo_magfie)
! Original: Order of coordinates is the following: x(1)=R (big radius),
! x(2)=phi (toroidal angle), x(3)=Z (altitude).
!
!
!  Input parameters:
!            formal:  x                -    array of coordinates
!  Output parameters:
!            formal:  bmod
!                     sqrtg
!                     bder
!                     hcovar
!                     hctrvr
!                     hcurl
!
!  Called routines:  GBhs,GBRZd
!
      USE magfield_mod, ONLY : ierrfield
      USE neo_magfie_mod, ONLY: neo_magfie, magfie_result, magfie_spline
      USE spline_settings, ONLY : isw_spl_fourier_cof, isw_eval_spl2d_der, &
           isw_eval_bcovars, flux_surf_dist
!
      implicit integer (i-n), double precision (a-h,o-z)
!
      INTEGER isw_neo_magfie
!
      DOUBLE PRECISION x(3),bmod,sqrtg,bder(3),hcovar(3),hctrvr(3),hcurl(3)
      DOUBLE PRECISION hr,hf,hz
!
      DOUBLE PRECISION ri,fii,zi,br,bf,bz, &
      BRR,BRF,BRZ,BFR,BFF,BFZ,BZR,BZF,BZZ, &
      BRK,BZK,BRRK,BRZK,BZRK,BZZK
!
      isw_neo_magfie = 1
      IF(isw_neo_magfie .EQ. 1) THEN
         ! evaluate radial covariant B-field component (1=on; 0=off)
         isw_eval_bcovars = 0
         ! evaluate derivatives from 2D periodic spline
         isw_eval_spl2d_der = 1
         ! spline Fourier coefficients radially (1=on; 0=off)
         isw_spl_fourier_cof = 0
         !flux_surf_dist = 10
         ! prepare 3D spline
         magfie_result = 0
         magfie_spline = 1
         !
         CALL neo_magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
         RETURN
      END IF
!
      rbig=MAX(x(1),1d-12)
!
!cccccc computation of gb in cylindrical co-ordinates cccccccc
      ri=rbig
      fii=x(2)
      zi=x(3)

! 19.03.2010 CALL GBhs(ri,fii,zi,br,bf,bz,BRR,BRF,BRZ,BFR,BFF,BFZ,BZR,BZF,BZZ)
      CALL gbtj2(ri,fii,zi,br,bf,bz,BRR,BRF,BRZ,BFR,BFF,BFZ,BZR,BZF,BZZ)
!
      IF(ierrfield.EQ.1) RETURN
! 18.03.2010      CALL GBRZd(ri,zi,BRK,BZK,BRRK,BRZK,BZRK,BZZK)
!
! Here we add the vertical field
! 18.03.2010      br=br+BRK
! 18.03.2010      bz=bz+BZK
!
! 18.03.2010      BRR=BRR+BRRK
! 18.03.2010      BRZ=BRZ+BRZK
! 18.03.2010      BZR=BZR+BZRK
! 18.03.2010      BZZ=BZZ+BZZK
!ccccc end of gb computation cccccccccc
      bmod=dsqrt(br**2+bf**2+bz**2)
      sqrtg=rbig
      hr=br/bmod
      hf=bf/bmod
      hz=bz/bmod
!
      bder(1)=(brr*hr+bfr*hf+bzr*hz)/bmod
      bder(2)=(brf*hr+bff*hf+bzf*hz)/bmod
      bder(3)=(brz*hr+bfz*hf+bzz*hz)/bmod
!
      hcovar(1)=hr
      hcovar(2)=hf*rbig
      hcovar(3)=hz
!
      hctrvr(1)=hr
      hctrvr(2)=hf/rbig
      hctrvr(3)=hz
!
      hcurl(1)=((bzf-rbig*bfz)/bmod                        &
     +          hcovar(2)*bder(3)-hcovar(3)*bder(2))/sqrtg
      hcurl(2)=((brz-bzr)/bmod                             &
     +          hcovar(3)*bder(1)-hcovar(1)*bder(3))/sqrtg
      hcurl(3)=((bf+rbig*bfr-brf)/bmod                     &
     +          hcovar(1)*bder(2)-hcovar(2)*bder(1))/sqrtg
!
      RETURN
      END
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine cyl_coord(x, x_cyl)

      use neo_magfie_mod, only: neo_cyl_coord => cyl_coord

      implicit none

      real(8), intent(in) :: x(3)       ! Flux coordinates s, phi and theta
      real(8), intent(out) :: x_cyl(3)  ! Cylindrical coordinates R, PHI, Z

      real(8) :: jacobian(3,3)          ! Jacobian, not used here

      call neo_cyl_coord(x, x_cyl, jacobian)  ! Call internal routine from neo_magfie.f90
end subroutine cyl_coord
