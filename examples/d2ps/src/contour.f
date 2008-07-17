module conmod
contains

SUBROUTINE sort_up(arr)
IMPLICIT NONE
double precision, DIMENSION(:), INTENT(INOUT) :: arr
INTEGER :: i,j,n
double precision :: a
n=size(arr)
do j=2,n
	a=arr(j)
	do i=j-1,1,-1
		if (arr(i) <= a) exit
		arr(i+1)=arr(i)
	end do
	arr(i+1)=a
end do
END SUBROUTINE sort_up

SUBROUTINE sort_down(arr)
IMPLICIT NONE
double precision, DIMENSION(:), INTENT(INOUT) :: arr
INTEGER :: i,j,n
double precision :: a
n=size(arr)
do j=2,n
	a=arr(j)
	do i=j-1,1,-1
		if (arr(i) >= a) exit
		arr(i+1)=arr(i)
	end do
	arr(i+1)=a
end do
END SUBROUTINE sort_down


SUBROUTINE resort(arr,irr)
IMPLICIT NONE
double precision, DIMENSION(:), INTENT(INOUT) :: arr
double precision, DIMENSION(:), allocatable :: brr
INTEGER :: k,n,irr(:)
n=size(arr)
allocate(brr(n))
brr=arr
do k=1,n
arr(k)=brr(irr(k))
end do
deallocate(brr)
END SUBROUTINE resort 

SUBROUTINE isort(arr,irr)
IMPLICIT NONE
integer, DIMENSION(:), INTENT(INOUT) :: arr
integer, DIMENSION(:), allocatable :: brr
INTEGER :: k,n,irr(:)
n=size(arr)
allocate(brr(n))
brr=arr
do k=1,n
arr(k)=brr(irr(k))
end do
deallocate(brr)
END SUBROUTINE isort 

SUBROUTINE sort_up2(arr,irr)
IMPLICIT NONE
double precision, DIMENSION(:), INTENT(INOUT) :: arr
INTEGER :: i,j,k,m,n,irr(:)
double precision :: a
n=size(arr)
do k=1,n
irr(k)=k
end do
do j=2,n
	a=arr(j)
	m=irr(j)
	do i=j-1,1,-1
		if (arr(i) <= a) exit
		arr(i+1)=arr(i)
		irr(i+1)=irr(i)
	end do
	arr(i+1)=a
	irr(i+1)=m
end do
END SUBROUTINE sort_up2

SUBROUTINE sort_down2(arr,irr)
IMPLICIT NONE
double precision, DIMENSION(:), INTENT(INOUT) :: arr
INTEGER :: i,j,k,m,n,irr(:)
double precision :: a
n=size(arr)
do k=1,n
irr(k)=k
end do
do j=2,n
	a=arr(j)
	m=irr(j)
	do i=j-1,1,-1
		if (arr(i) >= a) exit
		arr(i+1)=arr(i)
		irr(i+1)=irr(i)
	end do
	arr(i+1)=a
	irr(i+1)=m
end do
END SUBROUTINE sort_down2

double precision function value(c,xx,yy)
implicit double precision (a-h,o-z)
dimension c(4,4)
value=&
	(((((c(4,4)*yy+c(3,4))*yy+c(2,4))*yy+c(1,4))*xx &
	+(((c(4,3)*yy+c(3,3))*yy+c(2,3))*yy+c(1,3)))*xx &
	+(((c(4,2)*yy+c(3,2))*yy+c(2,2))*yy+c(1,2)))*xx &
	+(((c(4,1)*yy+c(3,1))*yy+c(2,1))*yy+c(1,1))
return
end function value

double precision function dvdy(c,xx,yy)
implicit double precision (a-h,o-z)
dimension c(4,4)
dvdy=&
	(((((c(4,4)*3*yy+c(3,4)*2)*yy+c(2,4)))*xx &
	+(((c(4,3)*3*yy+c(3,3)*2)*yy+c(2,3))))*xx &
	+(((c(4,2)*3*yy+c(3,2)*2)*yy+c(2,2))))*xx &
	+(((c(4,1)*3*yy+c(3,1)*2)*yy+c(2,1)))
return
end function dvdy 

double precision function dvdx(c,xx,yy)
implicit double precision (a-h,o-z)
dimension c(4,4)
dvdx=&
	((((c(4,4)*yy+c(3,4))*yy+c(2,4))*yy+c(1,4))*3*xx &
	+(((c(4,3)*yy+c(3,3))*yy+c(2,3))*yy+c(1,3))*2)*xx &
	+(((c(4,2)*yy+c(3,2))*yy+c(2,2))*yy+c(1,2))
return
end function dvdx 


end module conmod

subroutine square_con(q,xco,yco,cl,nc,ierr,idebug) 
use conmod
!implicit double precision (a-h,o-z)
implicit none 
type four
sequence
double precision,dimension(4) :: val,d1,d2,d12
end type four
type(four) q,xco,yco
double precision,dimension(4) :: e,xv,yv
double precision,dimension(4,4) :: c,cxco,cyco 
double precision,allocatable :: sr(:),vr(:),&
	xa(:),ya(:),va(:),xp(:,:),yp(:,:)
integer, allocatable :: ia(:),is(:),mb(:),np(:),ic(:),icl(:),ih(:),icsa(:) 
double precision r(3),xs(300),ys(300),xout(1500),yout(1500)
double precision x,y,v,vv,den,alf,fac,dx,dy,s,vx,vy,dummy,&
	angle,dangle,anglel,dist,arco,pi,bet,xt,yt,dyt,dxt,del
integer ns,nwall,nsl,nsadl,ntodo,iout,icolor,newmt,&
	i,j,k,m,n,mt,nz,nk,nc,newmb,nr,iside,idesperate,icyc,ierr,&
	idebug
double precision :: cl(nc)
data xv /0.,1.,1.,0./
data yv /0.,0.,1.,1./


nz=12*nc
pi=4*atan(1.)

allocate(sr(3*nc),vr(3*nc),ia(3*nc),icl(3*nc))
allocate(xa(nz),ya(nz),va(nz),is(nz),mb(nz),&
	xp(100,nz),yp(100,nz),np(nz),ic(nz),ih(nz),icsa(nz))

call   sort_up(cl)

call bcucof(q%val,q%d1,q%d2,q%d12,1.d0,1.d0,c)
call bcucof(xco%val,xco%d1,xco%d2,xco%d12,1.d0,1.d0,cxco)
call bcucof(yco%val,yco%d1,yco%d2,yco%d12,1.d0,1.d0,cyco)
call grid_box(xv,yv)
call find_boundary_intersect
if (nk.eq.0) then
	call one_color
	goto 2001
end if
ierr=10
ntodo=nk
mt=1
newmt=1
ih=0
icyc=0
do while (ntodo.gt.1)
	icyc=icyc+1
	if (icyc.gt.nk*nk) then
	ierr=1
	goto 2001
	end if
	if (newmt.ne.mt) then
		mt=newmt
	else
		mt=mod(mt,nk)+1
		newmt=mt
	end if
	m=mb(mt)
	write (79,'(i4,i4,f9.5,f9.5)', advance='yes') m,mt,va(m),va(mt)
	if (va(m).eq.va(mt).and.ih(mt).eq.0) then 
		call start_trip
		do while (nwall.lt.2)
			fac=min(2*fac,1.d0)
			call get_dx_dy; if (idebug.eq.1) write (79,*) " first";
			find_acceptable: do 
				anglel=atan2(dy,dx)
				x=x+dx
				y=y+dy
				if (idebug.eq.1) write(79,*)'x,y=',x,y,"before improve"
				call improve_root			
				call eval
				call get_dx_dy
				if (dx.eq.0) dy=ya(mt)-y
				if (dy.eq.0) dx=xa(mt)-x
				angle=atan2(dy,dx)
				dangle=min(abs(angle-anglel),2*pi-abs(angle-anglel))
				if (dangle.lt.pi/2.and.idebug.eq.1) write(79,*) "dangle=",dangle 
				if (dangle.lt.pi/2.) exit
				call jump_back
				if (idesperate.eq.1.and.idebug.eq.1) write(79,*) "desperate" 
				if (idesperate.eq.1) exit
			end do find_acceptable
			
			ns=ns+1
			xs(ns)=x
			ys(ns)=y
			if (idebug.eq.1) write (79,*) 'x,y=',x,y, " before chk" 
			
			call check_boundary	
			if (idebug.eq.1) write (79,*) 'x,y=',x,y, " after chk" 
			if (nwall.eq.2) then
				if (idebug.eq.1) write (79,*) "x,y=",x,y," nwall=2"
				if (abs(x).lt.0.001) then
				 x=0.; call improve_root_y;
				end if
				if (abs(1-x).lt.0.001) then
				 x=1.; call improve_root_y;
				end if
				if (abs(y).lt.0.001) then
				 y=0.; call improve_root_x;
				end if
				if (abs(1-y).lt.0.001) then
				 y=1.; call improve_root_x;
				end if
				dist=sqrt((x-xa(mt))**2+(y-ya(mt))**2)
				if (dist.lt.0.001) then
					call successful_trip
				else
					!write (*,*) ' wall, but not hit'
					write (79,'(a,4es10.3)') ' not hit',x,xa(mt),y,ya(mt)
				end if
			end if
			if (ns.ge.300) then
				write (79,'(a)') ' no wall'
				!write (*,*) m,mt,x,y
				stop
			end if
		end do
	else
		write (79,'(a)') ' do not try' 
	end if
end do
2001 continue
if (ierr.ne.0) then
if (ntodo.eq.2) then
	i=1
	do while (ih(i).eq.1.and.i.le.nk)
 		i=i+1
 	end do
	m=i
	do while (ih(i).eq.1.and.i.le.nk)
		 i=i+1
	 end do
	mt=i+1
	write (*,*) 'cheating with straight line',m,mt
	ns=2
	xs(1)=xa(m)
	ys(1)=ya(m)
	xs(2)=xa(mt)
	ys(2)=ya(mt)
	call successful_trip
else
	call one_color
end if
end if
deallocate(sr,vr,ia,icl)
deallocate(xa,ya,va,is,mb,&
	xp,yp,np,ic,ih,icsa)
return

contains
subroutine one_color
implicit none 
double precision qaver
xout(1)=0.
yout(1)=0.
xout(2)=1.
yout(2)=0.
xout(3)=1.
yout(3)=1.
xout(4)=0.
yout(4)=1.
iout=4
icolor=1
qaver=.25*(q%val(1)+q%val(2)+q%val(3)+q%val(4))

do i=1,nc
	if (cl(i).lt.qaver) icolor=i+1
end do
call ps_area(xout,yout,iout,icolor+2)
end subroutine one_color

subroutine jump_back
implicit none 
!write (*,'(a,i4,a,f7.3,a,f7.3)') 'saddle? ns=',ns,' x=',x,' y=',y
!!!if (nsl.eq.ns) nsadl=nsadl+1
nsadl=nsadl+1
if (idebug.eq.1) write (79,*) ' nsadl=', nsadl
x=xs(ns)
y=ys(ns)
fac=fac/4.
call eval
call get_dx_dy
if (nsadl.gt.10) then
	fac=0.1
	call get_dx_dy
	dummy=dx
	dx=dy
	dy=-dummy
	dx=(xa(mt)-x)*fac*abs(alf)
	dy=(ya(mt)-y)*fac*abs(alf)
	write (79,'(a,i4,a,2es11.3)',advance='yes') ' nsadl=',nsadl,' turn ',dx,dy
	nsadl=0	
	if (ns.eq.1) idesperate=1
end if 
nsl=ns
end subroutine jump_back

subroutine start_trip
implicit none 
x=xa(m)
y=ya(m)
v=va(m)
ns=1
xs(ns)=x
ys(ns)=y
alf=.05
call eval
if (idebug.eq.1) write (79,*) x,y,vv
if (idebug.eq.1) write (79,*) 'alf before=',alf
if (is(m).eq.4.and.vy.lt.0.) alf=-alf
if (is(m).eq.2.and.vy.gt.0.) alf=-alf
if (is(m).eq.1.and.vx.gt.0.) alf=-alf
if (is(m).eq.3.and.vx.lt.0.) alf=-alf
if (idebug.eq.1) write (79,*) 'alf  after=',alf
nwall=0
fac=1.
nsl=-99
nsadl=0
idesperate=0
end subroutine start_trip

subroutine eval
implicit none
vv=value(c,x,y)
vx=dvdx(c,x,y)
vy=dvdy(c,x,y)
end subroutine eval

subroutine get_dx_dy
implicit none
den=vx**2+vy**2
if (den.le.0.) then
	write (*,*) ' vx=0 and vy=0, stop'
	stop
end if
s=sqrt(den)
dx=fac*alf*vy/s
dy=-fac*alf*vx/s
if (idebug.eq.1) write (79,'(a,4es11.4)') 'dx,dy,x,y=',dx,dy,x,y
end subroutine get_dx_dy

subroutine successful_trip
implicit none 
ih(mt)=1
ih(m)=1
x=xa(mt)
y=ya(mt)
xs(ns)=x
ys(ns)=y
ntodo=ntodo-2
iout=0
do i=1,ns
	iout=iout+1
	xout(iout)=xs(i)
	yout(iout)=ys(i)
end do
icolor=0
if (v.lt.0.) icolor=1
call ps_line(xout,yout,iout,icolor)
do i=1,np(mt)
	iout=iout+1
	xout(iout)=xp(i,mt)
	yout(iout)=yp(i,mt)
end do
do i=1,iout
	!	write (*,'(a,f9.5,a,f9.5)') ' x=',xout(i),'   y=',yout(i)
end do
icsa(m)=icshft()
icolor=ic(m)+icsa(m)
write (79,*) ' success, ntodo=',ntodo,'  icolor=',icolor
write (79,'(20i2)') (ih(i),i=1,nk)
call ps_area(xout,yout,iout,icolor+2)
newmt=mod(mt,nk)+1
do while (ih(newmt).eq.1.and.ntodo.ne.0)
	newmt=mod(newmt,nk)+1
end do
newmb=mod(nk+m-2,nk)+1
do while (ih(newmb).eq.1.and.ntodo.ne.0)
	newmb=mod(nk+newmb-2,nk)+1
end do
write (79,*) 'newmb=',newmb,' ih=',ih(newmb),'newmt=',newmt,' ih=',ih(newmt)
mb(newmt)=newmb
do i=ns,1,-1 
	np(newmt)=np(newmt)+1
	xp(np(newmt),newmt)=xs(i)
	yp(np(newmt),newmt)=ys(i)
end do
if (newmt.ne.m) then
	do i=1,np(m)
		np(newmt)=np(newmt)+1
		xp(np(newmt),newmt)=xp(i,m)
		yp(np(newmt),newmt)=yp(i,m)
	end do
end if
if (ntodo.eq.0) then
	! write (*,*) ' ntodo=',ntodo
	iout=0
	do i=1,np(newmt)
		!write (*,'(a,f9.5,a,f9.5)') ' xp=',xp(i,newmt),'   yp=',yp(i,newmt)
		iout=iout+1
		xout(iout)=xp(i,newmt)
		yout(iout)=yp(i,newmt)
	end do
	icolor=ic(newmt)+1-icsa(newmt)
	call ps_area(xout,yout,iout,icolor+2)
	ierr=0
end if
write (79,'(a)') ' hit '
end subroutine successful_trip 

function icshft()
implicit none 
integer icshft
vx=dvdx(c,xout(1),yout(1))
vy=dvdy(c,xout(1),yout(1))
dummy=(xout(2)-xout(1))*vy-(yout(2)-yout(1))*vx
icshft=0
if (dummy.lt.0.) icshft=1 
end function icshft

subroutine find_boundary_intersect
implicit none 
nk=0
do iside=1,4
	!write (*,'(es10.3)') c
	if (iside.eq.1) call edgey(e,c,0.d0)
	if (iside.eq.2) call edgex(e,c,1.d0)
	if (iside.eq.3) call edgey(e,c,1.d0)
	if (iside.eq.4) call edgex(e,c,0.d0)
	nr=0.
	do i=1,nc
		call real_roots(e,r,cl(i),n)
		! write (*,'(3i4,4f9.5)') iside,nc,n,e !tempor
		do k=1,n
			nr=nr+1
			sr(nr)=r(k)
			vr(nr)=cl(i)
			icl(nr)=i
		end do
	end do
	if (iside.eq.1.or.iside.eq.2) then
		call   sort_up2(sr(1:nr),ia)
		call resort(vr(1:nr),ia)
		call isort(icl(1:nr),ia)
	end if
	if (iside.eq.3.or.iside.eq.4) then
		call sort_down2(sr(1:nr),ia)
		call resort(vr(1:nr),ia)
		call isort(icl(1:nr),ia)
	end if
	do i=1,nr
		nk=nk+1
		va(nk)=vr(i)
		is(nk)=iside
		ic(nk)=icl(i)
		if (iside.eq.1) then
			xa(nk)=sr(i)
			ya(nk)=0.
		end if
		if (iside.eq.2) then
			ya(nk)=sr(i)
			xa(nk)=1.
		end if
		if (iside.eq.3) then
			xa(nk)=sr(i)
			ya(nk)=1.
		end if
		if (iside.eq.4) then
			ya(nk)=sr(i)
			xa(nk)=0.
		end if
	end do
end do


if (mod(nk,2).ne.0) then
	write (*,*) ' nk=',nk,'...not even...stop'
end if

do i=2,nk
	mb(i)=i-1
	np(i)=0
end do
mb(1)=nk
np(1)=0 !tempor but OK?

	
do i=1,nk
	n=is(i)
	if (mb(i).lt.i) then
		do while (n.ne.is(mb(i))) 
			np(i)=np(i)+1
			xp(np(i),i)=xv(n)
			yp(np(i),i)=yv(n)
			n=n-1
			if (n.eq.0) n=4
		end do
	else
		n=n+4
		do while (n.ne.is(mb(i))) 
			np(i)=np(i)+1
			xp(np(i),i)=xv(mod(n-1,4)+1)
			yp(np(i),i)=yv(mod(n-1,4)+1)
			n=n-1
		end do
	end if
end do


do i=1,nk
	write (79,'(i4,3f9.4,4i5)') i,xa(i),ya(i),va(i),is(i),mb(i),np(i),ic(i)
	!do j=1,np(i)
		!write (*,*) xp(j,i),yp(j,i) 
	!end do
end do

end subroutine find_boundary_intersect


subroutine check_boundary
implicit none 
x=xs(ns-1)
y=ys(ns-1)
xt=xs(ns)
yt=ys(ns)
dx=xt-x
dy=yt-y

if (xt.lt.0.or.xt.gt.1.or.yt.lt.0.or.yt.gt.1.) then
	nwall=nwall+1
	if (xt.gt.1.) then
		dxt=min(1.-x,dx)
		dyt=dy*dxt/dx
		xt=x+dxt
		yt=y+dyt
		dx=dxt
		dy=dyt
		!x=xt; y=yt; call improve_root_y
	end if
	if (xt.lt.0.) then
		dxt=max(-x,dx)
		dyt=dy*dxt/dx
		xt=x+dxt
		yt=y+dyt
		dx=dxt
		dy=dyt
		!x=xt; y=yt; call improve_root_y
	end if
	x=xt
	y=yt
	if (yt.gt.1.) then
		dyt=min(1.-y,dy)
		dxt=dx*dyt/dy
		xt=x+dxt
		yt=y+dyt
		!x=xt; y=yt; call improve_root_x
	end if
	if (yt.lt.0.) then
		dyt=max(-y,dy)
		dxt=dx*dyt/dy
		xt=x+dxt
		yt=y+dyt
		!x=xt; y=yt; call improve_root_x
	end if
end if
x=xt
y=yt
call improve_root
xs(ns)=x
ys(ns)=y
end subroutine check_boundary

subroutine improve_root
implicit none 
do i=1,5 
	vv=value(c,x,y)
	vx=dvdx(c,x,y)
	vy=dvdy(c,x,y)
	den=vx**2+vy**2
	bet=-(vv-v)/den
	x=x+bet*vx
	y=y+bet*vy
end do
end subroutine improve_root

subroutine improve_root_x !not used
implicit none 
do i=1,5
	vv=value(c,x,y)
	vx=dvdx(c,x,y)
	x=x-(vv-v)/vx
end do
end subroutine improve_root_x

subroutine improve_root_y !not used
implicit none 
do i=1,5
	vv=value(c,x,y)
	vy=dvdy(c,x,y)
	y=y-(vv-v)/vy
end do
end subroutine improve_root_y


subroutine ps_area(x,y,n,icolor)
integer n,icolor
double precision x(n),y(n)
character jnum*4
write(jnum(2:4),'(i3.3)') icolor 
write(jnum(1:1),'(a)') 'C' 
write (97,'(a)') jnum 
write (97,'(f8.5,f8.5,a)') value(cxco,x(1),y(1)),value(cyco,x(1),y(1)),' M'
do i=2,n
	write (97,'(f8.5,f8.5,a)') value(cxco,x(i),y(i)),value(cyco,x(i),y(i)),' L'
end do
write (97,*) ' P'
end subroutine ps_area

subroutine ps_line(x,y,n,icolor)
integer n,icolor
double precision x(n),y(n)
character jnum*4
write(jnum(2:4),'(i3.3)') icolor 
write(jnum(1:1),'(a)') 'C' 
write (98,'(a)') jnum 
write (98,'(f8.5,f8.5,a)') value(cxco,x(1),y(1)),value(cyco,x(1),y(1)),' M'
do i=2,n
	write (98,'(f8.5,f8.5,a)') value(cxco,x(i),y(i)),value(cyco,x(i),y(i)),' L'
end do
write (98,*) ' S'
end subroutine ps_line

subroutine grid_box(x,y)
double precision x(4),y(4)
double precision xq,yq,xz,yz
do n=1,4
xq=x(n)
yq=y(n)
xz=x(mod(n,4)+1)-xq
yz=y(mod(n,4)+1)-yq
write (99,'(f8.5,f8.5,a)') value(cxco,xq,yq),value(cyco,xq,yq),' M'
do i=1,10
	write (99,'(f8.5,f8.5,a)') value(cxco,xq+.1*i*xz,yq+.1*i*yz),&
							   value(cyco,xq+.1*i*xz,yq+.1*i*yz),' L'
end do
end do
write (99,*) ' G'
end subroutine grid_box 
end subroutine square_con 

subroutine ps_area_old(x,y,n,icolor)
double precision x(n),y(n)
integer n,icolor
character jnum*4
write(jnum(2:4),'(i3.3)') icolor 
write(jnum(1:1),'(a)') 'C' 
write (97,'(a)') jnum 
write (97,'(f8.5,f8.5,a)') x(1),y(1),' M'
do i=2,n
	write (97,'(f8.5,f8.5,a)') x(i),y(i),' L'
end do
write (97,*) ' P'
end subroutine ps_area_old


subroutine edgey(e,c,y)
implicit double precision (a-h,o-z)
dimension  e(4),c(4,4)
do n=1,4
	e(n)=(c(1,n)+y*(c(2,n)+y*(c(3,n)+y*c(4,n))))
end do
return
end subroutine edgey

subroutine edgex(e,c,x)
implicit double precision (a-h,o-z)
dimension  e(4),c(4,4)
do n=1,4
	e(n)=(c(n,1)+x*(c(n,2)+x*(c(n,3)+x*c(n,4))))
end do
return
end subroutine edgex


SUBROUTINE BCUCOF(Y,Y1,Y2,Y12,D1,D2,CL)
implicit double precision (a-h,o-z)
DIMENSION Y(4),Y1(4),Y2(4),Y12(4),CL(16),X(16),WT(16,16)
DATA WT/1.,0.,-3.,2.,4*0.,-3.,0.,9.,-6.,2.,0.,-6.,&
	4.,8*0.,3.,0.,-9.,6.,-2.,0.,6.,-4.,10*0.,9.,-6.,&
	2*0.,-6.,4.,2*0.,3.,-2.,6*0.,-9.,6.,2*0.,6.,-4.,&
	4*0.,1.,0.,-3.,2.,-2.,0.,6.,-4.,1.,0.,-3.,2.,8*0.,&
	-1.,0.,3.,-2.,1.,0.,-3.,2.,10*0.,-3.,2.,2*0.,3.,&
	-2.,6*0.,3.,-2.,2*0.,-6.,4.,2*0.,3.,-2.,0.,1.,-2.,&
	1.,5*0.,-3.,6.,-3.,0.,2.,-4.,2.,9*0.,3.,-6.,3.,0.,&
	-2.,4.,-2.,10*0.,-3.,3.,2*0.,2.,-2.,2*0.,-1.,1.,&
	6*0.,3.,-3.,2*0.,-2.,2.,5*0.,1.,-2.,1.,0.,-2.,4.,&
	-2.,0.,1.,-2.,1.,9*0.,-1.,2.,-1.,0.,1.,-2.,1.,10*0.,&
	1.,-1.,2*0.,-1.,1.,6*0.,-1.,1.,2*0.,2.,-2.,2*0.,-1.,1./
D1D2=D1*D2
DO  I=1,4
	X(I)=Y(I)
	X(I+4)=Y1(I)*D1
	X(I+8)=Y2(I)*D2
	X(I+12)=Y12(I)*D1D2
END DO
DO  I=1,16
	CL(I)=&
		WT(I,1)*X(1)&
		+WT(I,2)*X(2)&
		+WT(I,3)*X(3)&
		+WT(I,4)*X(4)&
		+WT(I,5)*X(5)&
		+WT(I,6)*X(6)&
		+WT(I,7)*X(7)&
		+WT(I,8)*X(8)&
		+WT(I,9)*X(9)&
		+WT(I,10)*X(10)&
		+WT(I,11)*X(11)&
		+WT(I,12)*X(12)&
		+WT(I,13)*X(13)&
		+WT(I,14)*X(14)&
		+WT(I,15)*X(15)&
		+WT(I,16)*X(16)
end do
RETURN
END subroutine bcucof

subroutine real_roots(e,s,v,nn)
double precision :: r(6),e(4),ec(4),v,s(3)
call roots(e,r,v,n)
nn=0
do i=1,n
	if (r(2*i).eq.0.and.&
		r(2*i-1).ge.0.and.r(2*i-1).le.1.) then
		nn=nn+1
		s(nn)=r(2*i-1)
	end if
end do
if (nn.gt.1) call remove_double(s,nn)
do ii=1,nn-1
	do i=1,nn-1
		if (s(i).gt.s(i+1)) call exchange(s(i),s(i+1)) 
	end do
end do
return
end subroutine real_roots

subroutine remove_double(s,nn)
double precision:: s(3)
integer nn
if (nn.eq.2) then
	if (s(2).eq.s(1)) nn=0
else if (nn.eq.3) then
	if (s(2).eq.s(1)) then
		nn=1
		s(1)=s(3)
	else if (s(3).eq.s(1)) then
		s(1)=s(2)
		nn=1
	else if (s(2).eq.s(3)) then
		nn=1
	end if
end if
end subroutine remove_double

subroutine exchange(a,b)
double precision a,b,c
c=a
a=b
b=c
return
end subroutine exchange


subroutine roots(e,r,v,n)
double precision :: r(6),e(4),ec(4),v
ec=e
r=0.
n=0
ec(1)=ec(1)-v
!write (*,*) 'in roots ec(4)',ec
!if (sum(abs(ec(2:4))).lt.abs(ec(1))) return
if (abs(ec(4)).gt.1.e-8*maxval(abs(ec))) then 
	do n=1,4
		ec(n)=ec(n)/ec(4)
	end do
	call cubik (ec,r)
	n=3
else if (abs(ec(3)).gt.1.e-8*maxval(abs(ec))) then 
	call quadratic(ec,r)
	n=2
else if (abs(ec(2)).gt.1.e-8*maxval(abs(ec))) then 
	call linear(ec,r)
	n=1
else
	n=0
end if
end subroutine roots

subroutine cubik (c,x)
implicit double precision (a-h,o-z)
dimension  c(3),x(6)
! solve x**3+c(3)*x**2+c(2)*x+c(1)=0.	
! complex roots are x=x(1)+ix(2), etc.
q=(c(3)**2-3*c(2))/9.
r=(2*c(3)**3-9*c(3)*c(2)+27*c(1))/54.
if (r**2.lt.q**3) then
	!	 three real roots
	del=8*datan(1.d0)/3.
	sq=2*dsqrt(q)
	th3=dacos(r/dsqrt(q**3))/3.
	a3=c(3)/3.
	x(1)=-sq*dcos(th3)-a3
	x(2)=0.
	x(3)=-sq*dcos(th3+del)-a3
	x(4)=0.
	x(5)=-sq*dcos(th3-del)-a3
	x(6)=0.
else
	! one real and two complex conjugates
	a=-dsign(1.d0,r)*(dabs(r)+dsqrt(r**2-q**3))**(1./3.)
	b=0.
	if(a.ne.0.) b=q/a
	x(1)=(a+b)-c(3)/3.
	x(2)=0.
	x(3)=-.5*(a+b)-c(3)/3.
	x(4)=.5*dsqrt(3.d0)*(a-b)
	x(5)=x(3)
	x(6)=-x(4)
end if
return
end

subroutine quadratic (c,x)
implicit double precision (a-h,o-z)
dimension  c(3),x(4)
complex(kind(1.d0)) qq,rr
! solve c(3)*x**2+c(2)*x+c(1)=0.	
d=c(2)*c(2)-4*c(3)*c(1)
if (d.ge.0.) then
	q=-0.5*(c(2)+sign(sqrt(d),c(2)))
	x(1)=q/c(3)
	x(2)=0.
	x(3)=c(1)/q
	x(4)=0.
else
	qq=cmplx(-0.5*c(2),-0.5*sign(sqrt(-d),c(2)))
	rr=qq/c(3)
	x(1)=real(rr)
	x(2)=aimag(rr)
	rr=c(1)/qq
	x(3)=real(rr)
	x(4)=aimag(rr)
end if
return
end subroutine quadratic

subroutine linear(c,x)
implicit double precision (a-h,o-z)
dimension  c(2),x(2)
x(1)=-c(1)/c(2)
x(2)=0.
return
end subroutine linear


