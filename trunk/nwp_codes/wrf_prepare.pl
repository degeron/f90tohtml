#!/usr/bin/perl
#CHANGE THE FOLLOWING PATH TO YOUR WRF DIRECTORY
$the_path="/home/bfiedler/WRFV1/";

-e $the_path || die ("$the_path not set right");

$outdir="wrf_ls"; #relative path to output directory

mkdir($outdir,0755) if (! -e $outdir);

@the_F_dirs=(	"dyn_eh/", "dyn_em/", "dyn_exp/", "dyn_slt/", "frame/", "main/", "phys/", "share/");

@the_inc_dirs=(	"inc/");

$the_files="*.F";
foreach $dir (@the_F_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("crash 1\n");
}

$the_files="*.h";
foreach $dir (@the_inc_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("crash 2\n");
}

print "DONE\nNow try typing f90tohtml wrf.f2h\n";
print "(but you may later want to comment out troublesome file names in the .ls files)\n"

