 PROGRAM Area2
!---------------------------------------------------------------------
!
!  This program computes the area of a specific Shape given the input radius.

!     It computes the Area for r+dr and writes to "datos.dat" to plot Area(r)
!
!  Uses:  FUNCTION Area_Circle (r), FUNCTION Area_Sphere(r)
!
!---------------------------------------------------------------------
   IMPLICIT NONE

   INTERFACE
      FUNCTION Area_Circle (r)
        REAL ::  Area_Circle
        REAL, INTENT(IN) :: r
      END FUNCTION Area_Circle

      FUNCTION Area_Sphere (r)
        REAL :: Area_Sphere

        REAL, INTENT(IN) :: r
      END FUNCTION Area_Sphere
   END INTERFACE

! Declare local variables
   REAL :: radius, radius0, r0, dr, computed_Area
   INTEGER :: i, icase
   CHARACTER(10) :: shape

!  Prompt user for what case is interested
   write(*,*) "What case you want to solve? (Case 1: Circle, Case 2: Sphere"
   read(*,*) icase

!  Define the Geometrical figure   
   if (icase.eq.1) then
   shape = "Circle"
   else if (icase.eq.2) then
   shape = "Sphere"
   end if
 
   write(*, '(A)', ADVANCE = "NO") "Enter the radius of the circle or sphere:  "
   read(*,*) radius0
!  Open file to save data
   open(unit=5, file='datos.dat', status='unknown')

   write(*,*) "i, radius, computed_Area"
!  Compute the Areas for different radius (10 values)   
   r0 = radius0
   dr = 0.1
   do i = 1, 11
   radius = r0 + float(i-1)*dr

   if (icase.eq.1) then
!  The Area of Circle case
   computed_Area=Area_Circle(radius)
   else if (icase.eq.2) then
!  The case of Sphere case
   computed_Area=Area_Sphere(radius)
   end if
   write(*,*) i, radius, computed_Area
   write(5,*) radius, computed_Area
   end do

! Write out area using function call

   write(*,100) "Area of ", shape, " with radius ", radius, " is", &
            computed_Area   
100 format (A, 2x, A, 2x, A, 2x, F6.2, A, 2x, F11.2)
   close(unit=5)
   END PROGRAM Area

!-----Area_Circle----------------------------------------------------
!
!  Function to compute the area of a circle of given radius
!
!---------------------------------------------------------------------
 FUNCTION Area_Circle(r)

   IMPLICIT NONE
   REAL :: Area_Circle
   REAL, INTENT(IN) :: r

! Declare local constant Pi
   REAL, PARAMETER :: Pi = 3.1415927

        Area_Circle = Pi * r * r

 END FUNCTION Area_Circle

!-----Area_Sphere----------------------------------------------------
!
!  Function to compute the area of a sphere of given radius
!
!---------------------------------------------------------------------
 FUNCTION Area_Sphere(r)

   IMPLICIT NONE
   REAL :: Area_Sphere
   REAL, INTENT(IN) :: r

! Declare local constant Pi
   REAL, PARAMETER :: Pi = 3.1415927

   Area_Sphere = 4 * Pi * r * r

 END FUNCTION Area_Sphere
