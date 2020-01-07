

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
