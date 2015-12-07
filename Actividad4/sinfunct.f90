  program sinfunct

! Tres espacios de sangría

! No suponemos nada

   implicit none

 

! Definimos todas la variables que vamos a utilizar y su tipo

   integer :: i, npts ! npts es el número de puntos en el intervalo [0,2pi]

   real :: x, f_x,dx  ! La variable, una función f(x) y el incremento dx

   real, parameter :: pi = 4.0 * atan (1.0) ! Dejamos que la máquina calcule pi

   real, parameter :: epsilon = 1.0E-6 ! Tolerancia de cálculo

! Abrimos un archivo para giardar los datos de salida. 

   open(unit=11,file='seno.dat',Status='unknown')

 

   print *,  'Dame el número de puntos en el intervalo npts= '

   read(*,*) npts

 

   dx = (2.0 * pi) /float(npts) ! dx es el incremento en el eje x

   write(*,*) 'dx= ', dx

 

   x = 0.0 ! Es el límite inferior del intervalo de interés

! Comenzaremos evaluando f(x) desde x=0, y debemos incluir también x=2*pi

 

! Inciamos un "loop", notemos la sangría dentro del loop. !

   do i = 1, npts+1, 1 

       x = dx * float(i-1)

       f_x = sin(x)

       write(*,*) i, x , f_x  ! Escribe a la pantalla para dar seguimiento a lo que hace el programa

       write(11,*) x , f_x  ! Escribe x, f(x) en archivo para después graficar

!    Condicional: Pregunta si f(x) es igual a cero (Falso o Verdadero)

      if (abs(f_x) .le. epsilon) write(*,*) 'x =', x, ' es un cero de la función'

   enddo

! termina el loop

! Cerramos el archivo de escritura

   close(11)

   end program sinfunct 
