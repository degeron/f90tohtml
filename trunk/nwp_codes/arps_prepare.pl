#!/usr/bin/perl
#VERSION 0.5 7/14/00
#change $the_path to your arps4.5.1 directory
#then it is easy, type: arps_prepare.pl
#then look to see the files made in arps_ls
$the_path="/home/bfiedler/arps4.5.1/";

-e $the_path || die ("$the_path not set right");

$outdir="arps_ls"; #relative path to output directory
mkdir($outdir,0755) if (! -e $outdir);


@the_dirs=(	"src/adas/",
			"src/88d2arps/",
			"src/agri/",
			"src/arps/",
			"src/arps_mp/",
			"src/arpscvt/",
			"src/arpsdiff/",
			"src/arpsens/",
			"src/arpsextsnd/",
			"src/arpsintrp/",
			"src/arpsplt/",
			"src/arpsprt/",
			"src/arpssfc/",
			"src/arpssoil/",
			"src/arpstern/",
			"src/arpstintrp/",
			"src/arpstrn/",
			"src/assim/",
			"src/ext2arps/",
			"src/mci2arps/",
			"src/wtretcol/");
$the_files="*.[fF]";
foreach $dir (@the_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("cannot ls to $filename \n");
	&cleanse($file_name);
}
	$the_files="*.*";
	$file_name="$outdir/"."include.ls";
	$dir="include/";
	$ls_opt=$the_path.$dir.$the_files;
	system "ls $ls_opt >$file_name " || die ("cannot ls to $filename \n");

print "DONE\nNow try typing f90tohtml arps77.f2h\n";

sub cleanse{
# do not use dummy*.f or no*.f files
my ($file_name)=@_;
open (UNCLEAN,"<$file_name"); 
@all=<UNCLEAN>;
close UNCLEAN;
open (CLEAN,">$file_name"); 
foreach $line (@all){
	$last_part=$line;
	$last_part=~s/^.*\///;
#print $last_part;
	if ($last_part=~m/^dummy|^no/){
		print "not using:\n $line";
	}
	else{
		print CLEAN $line;}
	}
close CLEAN;
}
