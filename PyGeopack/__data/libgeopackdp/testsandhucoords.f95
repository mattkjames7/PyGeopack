program main
	
	real*8 l,m,r,x,y,z
	dimension x(13),y(13),z(13),l(13),m(13),r(13)
	integer i

	x = (/ 2.D0, 4.D0, 6.D0, 8.D0, 10.D0, 15.D0,-5.D0, 0.D0, 5.D0, 0.D0, 3.D0, 3.D0, 3.D0 /)
	y = (/ 0.D0, 0.D0, 0.D0, 0.D0,  0.D0,  0.D0, 0.D0,-5.D0, 0.D0, 5.D0, 0.D0, 0.D0, 0.D0 /)
	z = (/ 0.D0, 0.D0, 0.D0, 0.D0,  0.D0,  0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.5D0, 1.D0 /)

	
	call tomsandhucoords96(x,y,z,13,20120101,12.0,1,l,m,r)
	
	write(*,*) "*******************************************************"
	write(*,*) "Test along x"
	do i = 1,6,1
		write(*,'(A,F4.1,A,F4.1,A,F4.1,A)') "Pos = (",x(i),",",y(i),",",z(i),")"
		write(*,'(A,F4.1,A,F4.1,A,F4.2)') "L = ",l(i),", MLT = ",m(i),", R_norm = ",r(i)
	end do
	
	write(*,*) "*******************************************************"
	write(*,*) "Test around local time"
	i = 7
	write(*,'(A,F4.1,A,F4.1,A,F4.1,A)') "Pos = (",x(i),",",y(i),",",z(i),") (Midnight)"
	write(*,'(A,F4.2,A,F4.1,A,F4.2)') "L = ",l(i),", MLT = ",m(i),", R_norm = ",r(i)
	i = 8
	write(*,'(A,F4.1,A,F4.1,A,F4.1,A)') "Pos = (",x(i),",",y(i),",",z(i),") (Dawn)"
	write(*,'(A,F4.2,A,F4.1,A,F4.2)') "L = ",l(i),", MLT = ",m(i),", R_norm = ",r(i)
	i = 9
	write(*,'(A,F4.1,A,F4.1,A,F4.1,A)') "Pos = (",x(i),",",y(i),",",z(i),") (Noon)"
	write(*,'(A,F4.2,A,F4.1,A,F4.2)') "L = ",l(i),", MLT = ",m(i),", R_norm = ",r(i)
	i = 10
	write(*,'(A,F4.1,A,F4.1,A,F4.1,A)') "Pos = (",x(i),",",y(i),",",z(i),") (dusk)"
	write(*,'(A,F4.2,A,F4.1,A,F4.2)') "L = ",l(i),", MLT = ",m(i),", R_norm = ",r(i)
		
	write(*,*) "*******************************************************"
	write(*,*) "Test along z"
	do i = 11,13,1
		write(*,'(A,F4.1,A,F4.1,A,F4.1,A)') "Pos = (",x(i),",",y(i),",",z(i),")"
		write(*,'(A,F4.2,A,F4.1,A,F4.2)') "L = ",l(i),", MLT = ",m(i),", R_norm = ",r(i)
	end do

end program main

