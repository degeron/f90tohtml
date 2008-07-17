module all_var
integer, parameter :: mrl=kind(1.0d0) !mrl=8
integer id,nextra,im,jm,icmax,iplot,iw,jw,ixy,iusexy,itrick
character*80 datafile,psfile,ctab
character*80 desc_file,ctab_file
double precision, allocatable :: x(:,:),y(:,:),q(:,:),extra(:),z(:,:),conlev(:)
integer, allocatable :: colia(:,:)
double precision header(6),magic_number
double precision xs,ys,smin,smax,qmin,qmax,qavg,qrms,&
	xmin,xmax,ymin,ymax,xmn,xmx,ymn,ymx
character*6,allocatable :: cl(:)
character*5 adv
end module all_var
!***********************************************************
program d2ps 
use all_var
implicit double precision (a-h,o-z)
adv='yes'
magic_number=1.23d45

call read_input
call read_data
call make_eps_names
call open_ps_output
call diagnose_data
if (ixy.ge.0) call set_mx_mn
call set_scale
call set_size
call write_desc

if (iplot.eq.1.and.iusexy.ne.0)&
	call pixel_header(int(512*xs+.9999),int(512*ys+.9999))
if (iplot.ge.2)&
	call contour_header(int(512*xs+.9999),int(512*ys+.9999),iplot)

if (icmax.le.1.or.icmax.gt.1000) icmax=11 
allocate (cl(0:icmax+3))

call colors(iwkid,ctab,iblk,icmax)

if (iplot.eq.1.and.iusexy.eq.0) then
	if (im*jm.gt.10000) call big_im_plot
	if (im*jm.le.10000) call im_plot
end if

if (iplot.eq.1.and.iusexy.eq.1) call pixel_plot
if (iplot.ge.2) then
	nc=icmax-1 !number of contours
	allocate(conlev(nc))
	cint=(smax-smin)/nc ! the contour interval
	ca=smin+cint/2.  ! the first contour
	do ii=1,nc
		conlev(ii)	= cint*real(ii-1)+ca
	end do
	write (*,*) ' xs,ys=',xs,ys
	if (ixy.eq.0.and.iusexy.eq.1) then
		call cplot(q,x,y,im,jm,conlev,nc,0,xs,ys,xmn,xmx,ymn,ymx,iplot)
	else if (iusexy.eq.0.and.itrick.eq.1) then 
		call periodic_cplot(q,im,jm,conlev,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
	else if (iusexy.eq.0.and.itrick.eq.0) then 
		call noxy_cplot(q,im,jm,conlev,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
	else if (ixy.eq.1.and.iusexy.eq.1.and.itrick.eq.2) then
		call b_cplot(q,im,jm,x,y,in,jn,conlev,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
	else if (ixy.eq.1.and.iusexy.eq.1.and.itrick.eq.1) then
		call periodic_xycplot(q,im,jm,x,y,in,jn,conlev,nc,xs,ys,xmn,xmx,ymn,ymx,iplot)
	else
		write (*,*) ' invalid options with contour plot? stopping'
		stop
	end if
end if

call close_ps_output
write (*,'(2a)',advance=adv) '..write ',trim(psfile)

deallocate(q,colia)
im=2*(icmax-1)
jm=1
allocate(q(im,1),colia(im,1))
do i=1,im
	q(i,1)=smin+(smax-smin)*(i-.5)/(im)
end do
xs=1.
ys=.1
close (unit=97)
psfile=ctab_file
call open_ps_output
call im_plot
call close_ps_output
write (*,'(2a)') '..write ',trim(ctab_file)



contains

subroutine set_mx_mn
xmax=maxval(x)
xmin=minval(x)
ymax=maxval(y)
ymin=minval(y)
if (xmx-xmn.eq.0) then
	xmx=xmax
	xmn=xmin
end if
if (ymx-ymn.eq.0) then
	ymx=ymax
	ymn=ymin
end if
end subroutine set_mx_mn

subroutine read_input
open (unit=20,file='d2ps.input',status='old',err=100)
read (20,*) id,ixy,datafile,nextra,im,jm
read (20,*) xs,ys,xmn,xmx,ymn,ymx,ctab,icmax,smin,smax,psfile,iplot,iusexy,itrick
close (unit=20) 
if (ixy.lt.0.and.iusexy.eq.1) then
	write (*,*) ' ixy=',ixy,'  iusexy=',iusexy,'   stopping'
	stop
end if
if (iplot.eq.1.and.iusexy.eq.1.and.ixy.eq.0) then
	write (*,*) ' ixy=',ixy,'  iusexy=',iusexy,'  iplot=',iplot,'   stopping'
	stop
end if
return
100 write (*,*) ' no input file d2ps.input'
stop
end subroutine read_input

subroutine read_data
real headersp(6)
real, allocatable :: extrasp(:),qsp(:,:),xsp(:,:),ysp(:,:)

write (*,'(2a)',advance=adv)&
	' d2ras will open ',trim(datafile)
if (id.eq.0) then
	open (unit=19,file=trim(datafile),&
		STATUS= 'old',form='formatted',err=100)
	write (*,'(a)',advance=adv) '..opened formatted'
else
	open (unit=19,file=trim(datafile),&
		STATUS= 'old',form='unformatted',err=100)
	write (*,'(a,i2)',advance=adv) '..opened unformatted, id=',id
end if


if (nextra.eq.0) then !look for header
	if (id.eq.0) then
		read (19,*) header(1) 
	else if (id.eq.1) then
		read (19) headersp(1) 
		header(1)=headersp(1)
	else if (id.eq.2) then
		read (19) header(1) 
	else  
		write (*,*) ' id=',id,'  should be 0<id<2'
		stop
	end if
	write (*,*) ' header(1) was read'
	
	if (abs(header(1)-magic_number).gt.1.d38) then
		write (*,'(a,d15.5,d15.5,d15.5)',advance=adv)&
		 '..no header',magic_number,header(1),magic_number-header(1) 
	else
		rewind (unit=19)
		if (id.eq.0) then
			read (19,*) header
		else if (id.eq.1) then
			write (*,'(a)',advance=adv) ' reading single precision'
			read (19) headersp
			header=headersp
		else
			read (19) header
		end if
		nextra=header(2)
		im=header(3)
		jm=header(4)
		if (xs.le.0.0.or.xs.gt.1.0) xs=header(5)
		if (ys.le.0.0.or.ys.gt.1.0) ys=header(6)
	end if
end if

if (im.eq.0.or.jm.eq.0) then
	write (*,*) 'headers says im=',im,' jm=',jm	
	stop
end if
write (*,'(a,3i4)',advance=adv)'..',nextra,im,jm

allocate(extra(nextra),q(im,jm),colia(im,jm))
if (id.eq.1) allocate(extrasp(nextra),qsp(im,jm))

if (ixy.ge.0) then
	in=im+ixy
	jn=jm+ixy
	allocate(x(in,jn),y(in,jn))
	if (id.eq.1) allocate(xsp(in,jn),ysp(in,jn))
end if


rewind(19)
if (id.eq.0) then
	if (nextra.eq.0) read(19,*) q
	if (nextra.ne.0) read(19,*) extra,q
	if (ixy.ge.0) read (19,*) x,y
else if (id.eq.1) then
	write (*,*) "will read single precision"
	read(19) extrasp,qsp
	extra=extrasp
	q=qsp
	deallocate(extrasp,qsp)
	write (*,*) "did it ",extrasp
	if (ixy.ge.0) then
		read (19) xsp,ysp
		x=xsp
		y=ysp
		deallocate(xsp,ysp)
	end if
else 
	read(19) extra,q
	if (ixy.ge.0) read (19) x,y
end if
close (unit=19)
write (*,*) "returning"
return
100	write (*,*) 'no file ',trim(datafile)
stop
end subroutine read_data

subroutine make_eps_names
idot=scan(psfile,'.',back=.true.)-1
if (idot.le.0) idot=len(trim(psfile))
desc_file=psfile(1:idot)//'_desc.eps'
ctab_file=psfile(1:idot)//'_ctab.eps'
psfile=psfile(1:idot)//'.eps'
end subroutine make_eps_names

subroutine open_ps_output
open (unit=97,file=trim(psfile),&
	STATUS= 'unknown',form='formatted',err=100)
return
100 write (*,*) 'cannot open ',trim(psfile),' for output'
stop
end subroutine open_ps_output

subroutine close_ps_output
close (unit=97)
open (unit=94,file='catcom',status='unknown',form='formatted')
write (94,*) trim(psfile)
return
end subroutine close_ps_output

subroutine diagnose_data
qmin=minval(q)
qmax=maxval(q)
qavg=sum(q)/size(q)
qrms=sqrt(sum(q*q)/size(q)-qavg**2)
return
end subroutine diagnose_data


subroutine write_desc
character date_string*8,time_string*10
character*4 space
character*6,psc
call date_and_time(date_string,time_string)
space=achar(92)//'spc'
psc=') show'
open (unit=14,file=desc_file,status='unknown')
write (14,'(a)')&
	'%!PS-Adobe-2.0 EPSF-2.0',&
	'%%Creator: d2ps',&
	'%%DocumentFonts: Helvetica',&
	'%%BoundingBox: 0 0  512 96',& 
	'%%EndComments',&
	'/Helvetica 12 selectfont'
iw=0
jw=0
call wmt
write (14,'(a,a,a)') '(datafile=',trim(datafile),psc
iw=iw+1;call wmt
write (14,'(a,a,a)') '(psfile=',trim(psfile),psc
iw=iw+1;call wmt
write (14,'(a,a,a)') '(ps_created=',&
	trim(date_string)//' '//trim(time_string(1:4)),psc
iw=iw+1;call wmt
write (14,'(a,a,a)') '(ctab=',trim(ctab),psc
iw=iw+1;call wmt
write (14,'(a,es10.3,a)') '(xmin=',xmin,psc
call wmt
write (14,'(a,es10.3,a)') '(xmax=',xmax,psc
call wmt
write (14,'(a,es10.3,a)') '(ymin=',ymin,psc
call wmt
write (14,'(a,es10.3,a)') '(ymax=',ymax,psc
call wmt
write (14,'(a,es10.3,a)') '(xmn=',xmn,psc
call wmt
write (14,'(a,es10.3,a)') '(xmx=',xmx,psc
call wmt
write (14,'(a,es10.3,a)') '(ymn=',ymn,psc
call wmt
write (14,'(a,es10.3,a)') '(ymx=',ymx,psc
call wmt
write (14,'(a,es10.3,a)') '(qmin=',qmin,psc
call wmt
write (14,'(a,es10.3,a)') '(qmax=',qmax,psc
call wmt
write (14,'(a,es10.3,a)') '(qavg=',qavg,psc
call wmt
write (14,'(a,es10.3,a)') '(qrms=',qrms,psc
call wmt
write (14,'(a,es10.3,a)') '(smin=',smin,psc
iw=iw+2;call wmt
write (14,'(a,es10.3,a)') '(smax=',smax,psc
write (*,'(2a)',advance=adv) '..write ',trim(desc_file)
end subroutine write_desc

subroutine wmt
if (iw.gt.3) iw=0
if (iw.eq.0) jw=jw+1
write (14,'(i5,i5,a)')128*iw+2,98-16*jw,' moveto'
iw=iw+1
end subroutine wmt


subroutine set_scale
if (smin-smax.eq.0.) then
	smin=qmin
	smax=qmax
	if (smin.lt.0.0.and.smax.gt.0.0) then
		smax=max(abs(smin),smax)
		smin=-smax
	end if
end if
write (*,'(a,es9.2,es9.2)') '..',smin,smax
if (smin-smax.eq.0.) then
	write (*,'(a)',advance=adv) '..no range'
	stop
end if
end subroutine set_scale

subroutine set_size
if ((xs.eq.0.0.or.ys.eq.0.0).and.iusexy.eq.1) then
	xs=maxval(x)-minval(x)
	ys=maxval(y)-minval(y)
	if (ys/xs.gt.0.3.and.ys/xs.lt.3.0) then
		if (ys.gt.xs) then
			xs=xs/ys
			ys=1.
		else
			ys=ys/xs
			xs=1.
		end if
	else
		ys=1.
		xs=1.
	end if
end if
if (xs.le.0.0.or.xs.gt.1.0) xs=1.
if (ys.le.0.0.or.ys.gt.1.0) ys=1.
write (*,'(a,f6.3,f6.3)',advance=adv) '..scale ',xs,ys
end subroutine set_size

end program d2ps 


subroutine big_im_plot
! too avoid memory overflows, writes out the image in stipes
use all_var

!call colors(iwkid,ctab,iblk,icmax)
call raster(q,colia,smin,smax,im,jm,icmax)	

isize=512/im
jsize=512/jm
ib=im*isize
jb=jm*jsize

write(97,'(a)')&
	'%!PS-Adobe-2.0 EPSF-2.0',&
	'%%Creator: d2ps ',&
	'%%DocumentFonts: Helvetica'

write (97,'(a)',advance='no') '%%BoundingBox: 0 0 '
write (97,*) ib,jb 

write(97,'(a)')&
	'%%EndComments',&
	'%%EndProlog'

write (97,'(i5,1x,i5)',advance='no') isize,jsize
write (97,'(a)') ' scale'
do j=1,jm
	write (97,'(i5,1x,i5)',advance='no') 0,min(1,j-1) 
	write (97,'(a)') ' translate'
	
	
	write (97,'(a)') '<'
	
	do i=1,im
		write (97,'(a6)') cl(colia(i,j))
	end do
	
	write (97,'(a)') '>'
	
	write (97,'(i5,1x,i5)',advance='no') im,1
	write (97,'(a)')&
		' 8',&
		'[1 0 0 1 0 0]',&
		'{}',&
		'false',&
		'3',&
		'colorimage'
end do

write (97,'(i5,1x,i5)',advance='no') 0,-(jm-1)
write (97,'(a)') ' translate'
write (97,'(a)')&
	'0 setlinewidth',&
	'0 0'
write (97,*) im,jm 
write (97,'(a)') '%rectstroke'

end

subroutine im_plot
use all_var

!call colors(iwkid,ctab,iblk,icmax)
call raster(q,colia,smin,smax,im,jm,icmax)	

isize=xs*512/im
jsize=ys*512/jm
ib=im*isize
jb=jm*jsize

write(97,'(a)')&
	'%!PS-Adobe-2.0 EPSF-2.0',&
	'%%Creator: d2ps ',&
	'%%DocumentFonts: Helvetica'

write (97,'(a)',advance='no') '%%BoundingBox: 0 0 '
write (97,*) ib,jb 

write(97,'(a)')&
	'%%EndComments',&
	'%%EndProlog'

write (97,'(i5,1x,i5)',advance='no') isize,jsize
write (97,'(a)')&
	' scale',&
	'<'

do j=1,jm
	do i=1,im
		write (97,'(a6)') cl(colia(i,j))
	end do
end do

write (97,'(a)')&
	'>'

write (97,'(i5,1x,i5)',advance='no') im,jm
write (97,'(a)')&
	' 8',&
	'[1 0 0 1 0 0]',&
	'{}',&
	'false',&
	'3',&
	'colorimage'

write (97,'(a)')&
	'0 setlinewidth',&
	'0 0'
write (97,*) im,jm 
write (97,'(a)') 'rectstroke'

end

subroutine pixel_plot
!	(q,im,jm,smin,smax,xs,ys,ctab,icmax,&
!	colia,psfile,x,y)
use all_var
!real*4 q(im,jm),x(im+1,jm+1),y(im+1,jm+1)
!integer colia(im,jm)
!character psfile*80,ctab*80,jnum*3
character jnum*3
!call colors(iwkid,ctab,iblk,icmax)
call raster(q,colia,smin,smax,im,jm,icmax)	
xf=xs*12800/(xmx-xmn)
yf=ys*12800/(ymx-ymn)

do i=1,im
	do j=1,jm
		ibl=(x(i,j)-xmn)*xf
		ibr=(x(i+1,j)-xmn)*xf
		itl=(x(i,j+1)-xmn)*xf
		itr=(x(i+1,j+1)-xmn)*xf
		jbl=(y(i,j)-ymn)*yf
		jbr=(y(i+1,j)-ymn)*yf
		jtl=(y(i,j+1)-ymn)*yf
		jtr=(y(i+1,j+1)-ymn)*yf
		
		write(jnum,'(i3.3)') colia(i,j) 
		write (97,*) 'C'//jnum//' N'
		write (97,*) ibl,jbl,' M'
		write (97,*) ibr,jbr,' L'
		write (97,*) itr,jtr,' L'
		write (97,*) itl,jtl,' L P'
	end do
end do
end

subroutine gscr(i,j,rv,gv,bv)
use all_var  
character string*80,jnum*3,r*7,g*7,b*7
write(jnum,'(i3.3)') j
write(r(1:7),'(1x,f6.4)') rv 
write(g(1:7),'(1x,f6.4)') gv 
write(b(1:7),'(1x,f6.4)') bv 
string='/C'//jnum//' {'//r//g//b//' C} bind def'
if (iusexy.gt.0.or.iplot.eq.2) write (97,*) trim(string)
ir=255*rv
ig=255*gv
ib=255*bv
write (cl(j),'(z2.2,z2.2,z2.2)') ir,ig,ib 
return
end

subroutine hlsrgb(h,l,s,r,g,b)
real  h,l,s,r,g,b,v1,v2
l=l/100.
s=s/100.
if (l.le.0.5) then
	v2=l*(1+s)
else
	v2=l+s-l*s
end if
v1=2*l-v2
if (s.eq.0.) then
	r=l
	g=l
	b=l
else
	r=F_valux(v1,v2,h+120.)
	g=F_valux(v1,v2,h)
	b=F_valux(v1,v2,h-120.)
end if

contains

function F_valux(v1,v2,hue)
real v1,v2,hue,F_valux
if (hue.gt.360.) hue=hue-360.
if (hue.lt.0.) hue=hue+360.
if (hue.lt.60.) then
	F_valux=v1+(v2-v1)*hue/60.
else if (hue.lt.180.) then
	F_valux=v2
else if (hue.lt.240.) then
	F_valux=v1+(v2-v1)*(240.-hue)/60.
else
	F_valux=v1
end if
return
end function F_valux

end subroutine hlsrgb


subroutine colors(iwkid,ctab,iblk,icmax)
implicit real (a-h,o-z)
character*80 ctab

if (iblk.eq.0) then
	call gscr(iwkid,1,1.,1.,1.)
	call gscr(iwkid,0,0.,0.,0.)
else
	call gscr(iwkid,0,1.,1.,1.)
	call gscr(iwkid,1,0.,0.,0.)
end if
call gscr(iwkid,2,.8,1.,1.)
call gscr(iwkid,icmax+3,1.,1.,.8)

if (trim(ctab).eq.'cyan_black_yellow') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		w=(1-h)*2
		rv=max(0.,1.-w)
		gv=.7*4*(h-.5)*(h-.5)+.3
		bv=max(1.-(2*h),0.)
		rv=max(rv,.7*bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do

else if (trim(ctab).eq.'cyan_black_yellow_2') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		w=(1-h)*2
		rv=max(0.,1.-w**2)
		gv=.7*4*(h-.5)*(h-.5)+.3
		bv=max(1.-(2*h)**2,0.)
		rv=max(rv,.7*bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do

else if (trim(ctab).eq.'cyan_black_yellow_3') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		w=(1-h)*2
		rv=max(0.,1.-w**3)
		gv=.7*4*(h-.5)*(h-.5)+.3
		bv=max(1.-(2*h)**3,0.)
		rv=max(rv,.7*bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'cyan_black_yellow_x') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		f=.9
		w=(1-h)*2*f
		g=2*h*f
		rv=max(1.-w**3,0.)
		gv=.7*4*(h-.5)*(h-.5)+.3
		bv=max(1.-g**3,0.)
		rv=max(rv,.7*bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'blue_grey_orange') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		rv=h
		gv=.5
		bv=1-h
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
	
else if (trim(ctab).eq.'double_rainbow') then  
	do k = 1,icmax
		h = real(k)/icmax
		h=mod(2*h*360,360.)
		call hlsrgb(h,50.,100.,rv,gv,bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'fruit4') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		rv=2*h-h*h
		gv=.5*(1+cos(4*3.1415*h))*(1.-.25*sin(3.1415*h))
		bv=1-h*h
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'fruit3') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		g=h-.5
		factor=.9999+.5*exp(-100*.25)-.5*exp(-100*g*g)
		rv=(2*h-h*h)*factor
		gv=.5*(1+cos(4*3.1415*h))*(1.-.25*sin(3.1415*h))*factor
		bv=(1-h*h)*factor
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'rainbow') then  
	do k = 1,icmax
		h = real(k)/icmax
		h=h*360+176.
		call hlsrgb(h,50.,100.,rv,gv,bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'rainbow2') then  
	do k = 1,icmax
		h = real(k)/icmax
		g=0.5*(1-cos(3.141592*h))
		r=g*360+176.
		call hlsrgb(r,50.,100.,rv,gv,bv)
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'fruit2') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		g=h-.5
		factor=.9999+.5*exp(-200*.25)-.5*exp(-200*g*g)
		rv=(2*h-h*h)*factor
		gv=.5*(1+cos(4*3.1415*h))*(1.-.25*sin(3.1415*h))*factor
		bv=(1-h*h)*factor
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'fruit') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		g=h-.5
		factor=.9999+.5*exp(-200*.25)-.5*exp(-200*g*g)
		rv=(2*h-h*h)*factor
		bv=.5*(1+cos(4*3.1415*h))*(1.-.25*sin(3.1415*h))*factor
		gv=(1-h*h)*factor
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'grey_tanh2') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		g=h-.5
		tg=.25+.5*h+.2*(tanh(30*g)+tanh(15.))/(2*tanh(15.))
		rv=tg
		bv=tg
		gv=tg
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'grey_tanh') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		g=h-.5
		tg=(tanh(8*g)+tanh(4.))/(2*tanh(4.))
		rv=tg
		bv=tg
		gv=tg
		call gscr(iwkid,k+2,rv,gv,bv)
	end do

else if (trim(ctab).eq.'grey') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		rv=h
		bv=h
		gv=h
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'grey_short') then  
	do k = 1,icmax
		h = .3+.6*real(k-1)/(icmax-1.)
		rv=h
		bv=h
		gv=h
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
else if (trim(ctab).eq.'red_tanh') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		g=h-.5
		tg=tanh(8*g)+tanh(4.)
		factor=1./(2*tanh(4.))
		!rv=tg*factor
		rv=1.
		bv=tg*factor
		gv=tg*factor
		call gscr(iwkid,k+2,rv,gv,bv)
	end do
	
	
else if (trim(ctab).eq.'blue_white_red') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		green=min(1.9*h,1.9*(1-h))
		red=.95
		blue=.95
		if (h.gt.0.5) blue=green
		if (h.lt.0.5) red=green
		call gscr(iwkid,k+2,red,green,blue)
	end do
	
else if (trim(ctab).eq.'cyan_white_yellow') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		green=min(1.9*h,1.9*(1-h))
		red=.95
		blue=.95
		if (h.gt.0.5) blue=green
		if (h.lt.0.5) red=green
		green=max(green,1-1.9*(1-h))
		green=max(green,1-1.9*h)
		call gscr(iwkid,k+2,red,green,blue)
	end do
	
else if (trim(ctab).eq.'blue_black_red') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		green=0.
		blue=max(1-1.3*h,0.)
		red=max(1-1.3*(1.-h),0.)
		call gscr(iwkid,k+2,red,green,blue)
	end do

else if (trim(ctab).eq.'green') then  
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		green=.18+.80*(-h*h+2*h)
		red=.18+.80*h
		blue=.18+.80*h*h
		call gscr(iwkid,k+2,red,green,blue)
	end do
	
else if (trim(ctab).eq.'red') then  
	do k = 1,icmax
		h = real(k-1)/(icmax-1.)
		red=.18+.80*(-h*h+2*h)
		blue=.18+.80*h
		green=.18+.80*h*h
		call gscr(iwkid,k+2,red,green,blue)
	end do
	
else if (trim(ctab).eq.'blue') then  
	do k = 1,icmax
		h = real(k-1)/(icmax-1.)
		blue=.18+.80*(-h*h+2*h)
		green=.18+.80*h
		red=.18+.80*h*h
		call gscr(iwkid,k+2,red,green,blue)
	end do
	
else   !default 
	do k = 1,icmax
		h = (k-1.)/(icmax-1.)
		green=min(1.9*h,1.9*(1-h))
		red=.95
		blue=.95
		if (h.gt.0.5) blue=green
		if (h.lt.0.5) red=green
		green=max(green,1-1.9*(1-h))
		green=max(green,1-1.9*h)
		call gscr(iwkid,k+2,red,green,blue)
	end do
	
	
end if

return
end

subroutine raster(z,image,zmin,zmax,in,jn,icmax)	

implicit double precision (a-h,o-z)
dimension z(in,jn)
integer image(in,jn)
zzmax=1.00000*zmax
fac=real(icmax-1)/(zzmax-zmin)
do j=1,jn
	do i=1,in
		ival=int((z(i,j)-zmin)*fac+3.5)
		if (z(i,j).lt.zmin) ival=2
		if (z(i,j).gt.zzmax) ival=icmax+3
		image(i,j)=ival
	end do
end do
return
end

subroutine pixel_header(ib,jb)
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
	'% three choices follow: color and grid, color only, grid only',&
	'/P {closepath gsave fill grestore 0 0 0 setrgbcolor stroke} bind def',&
	'%/P {closepath gsave fill grestore} bind def',&
	'%/P {closepath gsave grestore 0 0 0 setrgbcolor stroke} bind def',&
	'%%EndProlog',&
	'0 setlinewidth',&
	'0 0 translate',&
	'0.040 0.040 scale',&
	'0 setgray'

return
end

