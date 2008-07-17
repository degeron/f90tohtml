#!/usr/bin/perl
#5/31/2002
#change the following ABSOLUTE PATH to your CRM directory
$the_path="/home/bfiedler/f90tohtml/examples/crm-1.13/";

-e $the_path || die ("$the_path not set right");

$outdir="crm_ls"; #relative path to output directory
mkdir($outdir,0755) if (! -e $outdir);


@the_dirs=(	"",
			"src/"
			);

$the_files="*.[fF]";
foreach $dir (@the_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	if ($title eq "") { $title="main"};
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("dammit \n");
}
$file_name="$outdir/include.ls";
unlink($file_name) || warn ("cannot unlink $file_name");
$the_files="*.h";
foreach $dir (@the_dirs){
	$ls_opt=$the_path.$dir.$the_files;
	print "\nappending to $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >>$file_name " || die ("dammit 3\n");
}
print "DONE\nNow try typing f90tohtml crm.f2h\n";

