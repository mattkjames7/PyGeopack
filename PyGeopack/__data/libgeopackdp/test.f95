program main
	
	real*8 Bx,By,Bz
	
	call tom96(3.D0,3.D0,0.D0,1,20120101,12.0,Bx,By,Bz)
	
	write(*,*) Bx,By,Bz

end program main

