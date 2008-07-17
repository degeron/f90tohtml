#!/usr/bin/perl
#CHANGE THE FOLLOWING PATH TO YOUR COAMPS2.0 DIRECTORY
$the_path="/nethome/bfiedler/cvscoamps/coamps2c";
#ALSO CHANGE THE NAME OF THE MOD DIRECTORY IN @the_dirs

-e $the_path || die ("$the_path not set right");

$outdir="coamps_ls"; #relative path to output directory
mkdir($outdir,0755) if (! -e $outdir);

#VERSION 0.4 4/21/98

@the_dirs=(	"/src/newdtg/",
			"/src/coamps_forecast/",
			"/src/coamps_analysis/",
			"/libsrc/coampslib/",
			"/libsrc/fishpaklib/",
			"/libsrc/fnoclib/",
			"/libsrc/nl_beqlib/",
			"/libsrc/oilib/");

@the_prol_dirs=(	"/prologues/coampslib/",
                        "/prologues/main/",
                        "/prologues/oilib/"),
@the_h_dirs=(			"/libsrc/coampslib/",
						"/libsrc/nl_beqlib/",
						"/libsrc/oilib/");
$the_files="*.[fF]";
foreach $dir (@the_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("crash 1\n");
}
print "\n\nNow prepare special file 'include.ls' for COAMPS includes:\n";
$file_name="$outdir/include.ls";
unlink($file_name) || warn ("cannot unlink $file_name");
$the_files="*.prol";
foreach $dir (@the_prol_dirs){
	$ls_opt=$the_path.$dir.$the_files;
	print "\nappending to $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >>$file_name " || die ("crash 2\n");
}
$the_files="*.h";
foreach $dir (@the_h_dirs){
	$ls_opt=$the_path.$dir.$the_files;
	print "\nappending to $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >>$file_name " || die ("crash 3\n");
}
print "DONE\nNow try typing f90tohtml coamps.f2h\n";

