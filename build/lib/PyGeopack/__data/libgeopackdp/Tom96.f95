

subroutine Tom96(x,y,z,n,D,ut,bx,by,bz) 

	implicit none
	real*8 x,y,z,bx,by,bz
	real*4 ut
	integer n,D
	dimension x(n),y(n),z(n),bx(n),by(n),bz(n)

	!Call the C code (hopefully!)
	call Tom(x,y,z,n,D,ut,bx,by,bz)
	
	return
end


subroutine TomSandhuCoords96(x,y,z,n,D,ut,verbose,l,m,r) 

	implicit none
	real*8 x,y,z,l,m,r
	real*4 ut
	integer n,D,verbose
	dimension x(n),y(n),z(n),l(n),m(n),r(n)

	!Call the C code (hopefully!)
	call sandhucoordsf(x,y,z,n,D,ut,verbose,l,m,r)
	
	return
end
