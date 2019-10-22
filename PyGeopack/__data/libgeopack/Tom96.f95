

subroutine Tom96(x,y,z,n,D,ut,bx,by,bz) 

	implicit none
	real x,y,z,ut,bx,by,bz
	integer n,D
	dimension x(n),y(n),z(n),bx(n),by(n),bz(n)

	!Call the C code (hopefully!)
	call Tom(x,y,z,n,D,ut,bx,by,bz)
	
	return
end
