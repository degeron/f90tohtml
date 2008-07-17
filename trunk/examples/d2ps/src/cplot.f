subroutine open_extra_ps
open (unit=98,file='teMp.eps',status='unknown',form='formatted',err=101)
open (unit=99,file='tEmp.eps',status='unknown',form='formatted',err=102)
write (99,'(a)') 'GCOLOR' 
return
101 write (*,*) 'cannot open teMp.eps for output'
102 write (*,*) 'cannot open tEmp.eps for output'
stop
end subroutine open_extra_ps

subroutine close_extra_ps
close (unit=98)
write (99,'(a)') &
'.001953125 .001953125  scale',&
'/Helvetica 12 selectfont',&
'%uncomment next four lines for example labels',&
'%105 105 BX',&
'%(this) show',&
'%305 205 BX',&
'%(that) show'
close (unit=99)
return
end subroutine close_extra_ps

subroutine periodic_cplot(b,im,jm,cl,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
double precision b(im,jm),cl(nc),xs,ys,xmn,xmx,ymn,ymx
double precision, allocatable :: xa(:,:),ya(:,:),bb(:,:)
in=im+1
jn=jm+1
allocate(xa(in,jn),ya(in,jn),bb(in,jn))

write (*,*) ' using trick to add extra row & column to periodic data'

do i=1,im
do j=1,jm
bb(i,j)=b(i,j)
end do
end do

do i=1,im
bb(in,i)=b(1,i)
bb(i,jn)=b(i,1)
end do
bb(in,in)=b(1,1)

do i=1,in
do j=1,jn
xa(i,j)=(i-1.)/im
ya(i,j)=(j-1.)/jm
end do
end do

open (unit=79,file='contour_log.dat',status='unknown')

call cplot(bb,xa,ya,in,jn,cl,nc,1,xs,ys,xmn,xmx,ymn,ymx,iplot)

deallocate(xa,ya,bb)

end subroutine periodic_cplot

subroutine noxy_cplot(b,im,jm,cl,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
double precision b(im,jm),cl(nc),xs,ys,xmn,xmx,ymn,ymx
double precision, allocatable :: xa(:,:),ya(:,:)
allocate(xa(im,jm),ya(im,jm))
write (*,*) 'faking x and y'
do i=1,im
do j=1,jm
xa(i,j)=(i-1.)/(im-1)
ya(i,j)=(j-1.)/(jm-1)
end do
end do
open (unit=79,file='contour_log.dat',status='unknown')
call cplot(b,xa,ya,im,jm,cl,nc,1,xs,ys,xmn,xmx,ymn,ymx,iplot)
deallocate(xa,ya)
end subroutine noxy_cplot

subroutine periodic_xycplot(b,im,jm,x,y,in,jn,cl,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
double precision b(im,jm),x(in,jn),y(in,jn),cl(nc),xs,ys,xmn,xmx,ymn,ymx
double precision, allocatable :: xa(:,:),ya(:,:),bb(:,:)
ip=im+2
jp=jm+2
allocate(xa(ip,jp),ya(ip,jp),bb(ip,jp))

do i=1,im
do j=1,jm
bb(i+1,j+1)=b(i,j)
xa(i+1,j+1)=.25*(x(i,j)+x(i+1,j)+x(i,j+1)+x(i+1,j+1))
ya(i+1,j+1)=.25*(y(i,j)+y(i+1,j)+y(i,j+1)+y(i+1,j+1))
end do
end do

do j=2,jp-1
xa(1,j)=.5*(x(1,j)+x(1,j-1))
ya(1,j)=.5*(y(1,j)+y(1,j-1))
xa(ip,j)=.5*(x(in,j)+x(in,j-1))
ya(ip,j)=.5*(y(in,j)+y(in,j-1))
bb(1,j)=.5*(b(1,j-1)+b(im,j-1))
bb(ip,j)=bb(1,j)
end do

do i=2,ip-1
xa(i,1)=.5*(x(i,1)+x(i-1,1))
ya(i,1)=.5*(y(i,1)+y(i-1,1))
xa(i,jp)=.5*(x(i,jn)+x(i-1,jn))
ya(i,jp)=.5*(y(i,jn)+y(i-1,jn))
bb(i,1)=.5*(b(i-1,1)+b(i-1,jm))
bb(i,jp)=bb(i,1)
end do

xa(1,1)=x(1,1)
ya(1,1)=y(1,1)
xa(ip,1)=x(in,1)
ya(ip,1)=y(in,1)
xa(1,jp)=x(1,jn)
ya(1,jp)=y(1,jn)
xa(ip,jp)=x(in,jn)
ya(ip,jp)=y(in,jn)

bb(1,1)=.5*(bb(2,1)+bb(in,1))
bb(1,jp)=bb(1,1)
bb(ip,jp)=bb(1,1)
bb(ip,1)=bb(1,1)

!do i=1,ip
!write (*,'(3es10.3)') xa(i,jp),ya(i,jp),bb(i,jp)
!end do


open (unit=79,file='contour_log.dat',status='unknown')

call cplot(bb,xa,ya,ip,jp,cl,nc,1,xs,ys,xmn,xmx,ymn,ymx,iplot)

deallocate(xa,ya,bb)

end subroutine periodic_xycplot

subroutine b_cplot(b,im,jm,x,y,in,jn,cl,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
double precision b(im,jm),x(in,jn),y(in,jn),cl(nc),xs,ys,xmn,xmx,ymn,ymx
double precision, allocatable :: xa(:,:),ya(:,:),bb(:,:)
ip=im+2
jp=jm+2
allocate(xa(ip,jp),ya(ip,jp),bb(ip,jp))


do i=1,im
do j=1,jm
bb(i+1,j+1)=b(i,j)
xa(i+1,j+1)=.25*(x(i,j)+x(i+1,j)+x(i,j+1)+x(i+1,j+1))
ya(i+1,j+1)=.25*(y(i,j)+y(i+1,j)+y(i,j+1)+y(i+1,j+1))
end do
end do

do j=2,jp-1
xa(1,j)=x(1,1)
ya(1,j)=ya(2,j)
bb(1,j)=bb(2,j)
xa(ip,j)=x(in,1)
ya(ip,j)=ya(ip-1,j)
bb(ip,j)=bb(ip-1,j)
end do

do i=1,ip
ya(i,1)=y(1,1)
xa(i,1)=xa(i,2)
bb(i,1)=bb(i,2)
ya(i,jp)=y(1,jn)
xa(i,jp)=xa(i,jp-1)
bb(i,jp)=bb(i,jp-1)
end do


open (unit=79,file='contour_log.dat',status='unknown')

call cplot(bb,xa,ya,ip,jp,cl,nc,0,xs,ys,xmn,xmx,ymn,ymx,iplot)

deallocate(xa,ya,bb)

end subroutine b_cplot

subroutine cplot(bb,xa,ya,in,jn,cl,nc,iper,xs,ys,xmn,xmx,ymn,ymx,iplot)
double precision bb(in,jn),cl(nc),xa(in,jn),ya(in,jn),xs,ys,xmn,xmx,ymn,ymx
type four
sequence
double precision,dimension(4) :: val,d1,d2,d12
end type four

type(four) x,y,q

call open_extra_ps

!xmin=minval(xa)
!ymin=minval(ya)
!xa=xa-xmin
!ya=ya-ymin
!xmax=maxval(xa)
!ymax=maxval(ya)
!xa=xs*xa/xmax
!ya=ys*ya/ymax

xa=xs*(xa-xmn)/(xmx-xmn)
ya=ys*(ya-ymn)/(ymx-ymn)


im=in-1
jm=jn-1

do i=1,im
write (*,*) ' i=',i
do j=1,jm
ip=i+1
jp=j+1
ir=ip+1
il=i-1
jt=jp+1
jb=j-1
rdy=.5
rdyp=.5
rdx=.5
rdxp=.5
if (ir.gt.in) ir=2
if (il.lt.1) il=im
if (jt.gt.jn) jt=2
if (jb.lt.1) jb=jm
if (iper.eq.1) call prep(q,bb)
ip=i+1
jp=j+1
ir=ip+1
il=i-1
jt=jp+1
jb=j-1
rdy=.5
rdyp=.5
rdx=.5
rdxp=.5
if (ir.gt.in) then
ir=in
rdxp=1.
end if
if (il.lt.1) then
il=1
rdx=1.
end if
if (jt.gt.jn) then
jt=jn
rdyp=1.
end if
if (jb.lt.1) then
jb=1
rdy=1.
end if

call prep(x,xa)
call prep(y,ya)
if (iper.ne.1) call prep(q,bb)
ierr=0
idebug=1
!if (i.eq.20.and.j.ge.23) idebug=1
write (79,'(a,i4,i4)')' i,j=',i,j,' idebug=',idebug
!if (i.eq.20.and.j.eq.23 )call square_con(q,x,y,cl,nc,ierr,idebug) 
!!!call lq_square_con(q,x,y,cl,nc,ierr,idebug) !temporary 
call square_con(q,x,y,cl,nc,ierr,idebug) 
if (ierr.ne.0) then
write (*,*) ' ierr=',ierr,' at i,j=',i,j
!close (unit=79)
!close (unit=89)
!stop
end if
end do
end do

call close_extra_ps

contains

subroutine prep(r,v)
double precision v(in,jn)
type(four) r
r%val(1)=v(i,j)
r%val(2)=v(ip,j)
r%val(3)=v(ip,jp)
r%val(4)=v(i,jp)
r%d1(1)=(v(ip,j)-v(il,j))*rdx
r%d1(2)=(v(ir,j)-v(i ,j))*rdxp
r%d1(3)=(v(ir,jp)-v(i ,jp))*rdxp
r%d1(4)=(v(ip,jp)-v(il,jp))*rdx
r%d2(1)=(v(i ,jp)-v(i ,jb))*rdy
r%d2(2)=(v(ip,jp)-v(ip,jb))*rdy
r%d2(3)=(v(ip,jt)-v(ip,j ))*rdyp
r%d2(4)=(v(i ,jt)-v(i ,j ))*rdyp
r%d12(1)=(v(ip,jp)+v(il,jb)-v(il,jp)-v(ip,jb))*rdx*rdy
r%d12(2)=(v(ir,jp)+v(i ,jb)-v(i ,jp)-v(ir,jb))*rdxp*rdy
r%d12(3)=(v(ir,jt)+v(i ,j )-v(i ,jt)-v(ir,j ))*rdxp*rdyp
r%d12(4)=(v(ip,jt)+v(il,j )-v(il,jt)-v(ip,j ))*rdx*rdyp
end subroutine prep


end subroutine cplot 

subroutine contour_header(ib,jb,iplot)
character conyes*1, gridyes*1 
conyes=''
gridyes=''
if (iplot.eq.3.or.iplot.eq.5) conyes='%'
if (iplot.ge.4) gridyes='%'
write (*,*) iplot,conyes,gridyes
write (97,'(a)') &
	'%!PS-Adobe-2.0 EPSF-2.0',&
	'%%Creator: d2ps',&
	'%%DocumentFonts: Helvetica'

write (97,'(a)',advance='no') '%%BoundingBox: 0 0 '
write (97,*) ib,jb

write (97,'(a)') &
	'%%EndComments',&
	'/M {moveto} bind def',&
	'/L {lineto} bind def',&
	'/R {rmoveto} bind def',&
	'/V {rlineto} bind def',&
	'/C {setrgbcolor} bind def',&
	'/N {newpath} bind def',&
	'/S {stroke} bind def',&
	conyes//'/S {newpath} bind def %uncomment to NOT stroke contours',&
	'/P {closepath  fill  newpath} bind def',&
	'%/P {newpath} bind def  %uncomment to NOT fill regions',&
	'/G {stroke} bind def',&
	gridyes//'/G {newpath} bind def %uncomment to NOT stroke grid',&
	'/GCOLOR {  0.0000 1.0000 0.0000 C} bind def',&
	'/BX {M save C001 24 0 V 0 12 V -24 0 V P restore 0 1 R C000} bind def %used for labels, see end of file',&
	'%%EndProlog',&
	'.001 setlinewidth',&
	'0 0 translate',&
	'512 512 scale',&
	'0 setgray',&
	'N',&
	'%color table will follow.',&
	'%C001 is neg. cont. color, c000 is pos. cont., next 2 are not used, rest for color fill'

return
end subroutine contour_header

