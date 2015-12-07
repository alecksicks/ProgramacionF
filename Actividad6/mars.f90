program mars

  IMPLICIT NONE

  
  INTEGER :: i, t
  REAL :: Df, dx
  COMPLEX :: r_T, r_M, D
  REAL, parameter :: pi = 4.0 * atan (1.0), dm=1.523662
  COMPLEX, parameter :: c = (0.0,1.0)


print *, 'Ingresa el tiempo'
read *, t

dx= 0.01
open(unit=15, file='mars.dat')
do i=1, t, 1

   
   r_T=(cexp(2.0*pi*c*dx*float(i)))
   r_M= ((cexp(2.0*pi*c*(dm**(3/2))*dx*float(i)))*dm)
   D= abs(r_T - r_M)
   Df= REAL(csqrt(D*conjg(D)))

   
   print *, 'La distancia entre la Tierra y Marte en un tiempo  ',i, 'es:&
        &', Df,'UA'
   write(11,*) i, Df
enddo

end program mars
