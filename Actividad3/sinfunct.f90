program taylor
  IMPLICIT NONE

  INTERFACE
     FUNCTION TaylorSerie(f_0,f_1,f_2,h)
       REAL :: SerieTaylor
       REAL, INTENT(IN) :: f_0, f_1, f_2, h
     END FUNCTION TaylorSerie
  END INTERFACE
  REAL :: x_0, x_1, x_2, h, sum, F
  INTEGER :: i, npts, casoi
  
  write(*,*) "Ingresa el numero correspondiente para la funcion deseada, seno: 1 ,coseno: 2 ,tangente: 3 ,exponencial: 4."
  read(*,*) casoi
  write(*,*) casoi

  
  npts=21
     write(*,*) "Serie de Taylor del caso seleccionado"
     h=0.1
     do i=1, npts
        x_0=float(i-1)*h
        x_1=float(i)*h
        x_2=float(i+1)*h
       sum =Taylor(F(x_0,casoi), F(x_1,casoi), F(x_2,casoi), h)
       write(*,*) i, suma, F(x_1,casoi), (sum-F(x_1,casoi))
      
          
       open(unit=12, file='Error.dat')
       write(12,*)i, sum-F(x_1,casoi)
       
     end do
    close(12)

   END PROGRAM taylor
   
   FUNCTION TaylorSerie(f_0,f_1,f_2,h)
     IMPLICIT NONE
     REAL :: TaylorSerie
     REAL, INTENT(IN) :: f_0, f_1, f_2, h

     SerieTaylor=f_1+((f_2-f_0)/(2*h))*h+((f_2-2*f_1+f_0)/(2*h*h))*h*h
     END FUNCTION TaylorSerie

     FUNCTION F(x,casoi)
       IMPLICIT NONE
       REAL :: F
       REAL, INTENT(IN) ::  x
       INTEGER, INTENT(IN) :: casoi
       if (casoi.EQ.1) then
          F=sin(x)
       end if
       
       if (casoi.EQ.2) then
          F=cos(x)
       end if

       if (casoi.EQ.3) then
          F=tan(x)
       end if

       if (casoi.EQ.4) then
          F=exp(x)
       end if
       
     END FUNCTION F
