#!/usr/bin/perl
#CHANGE THE FOLLOWING PATH TO YOUR MM5  DIRECTORY
$the_path="/home/bfiedler/downloads/MM5/";

-e $the_path || die ("$the_path not set right");

$outdir="mm5_ls"; #relative path to output directory
mkdir($outdir,0755) if (! -e $outdir);

#VERSION 0.4 4/21/98

@the_dirs=("pick/");
#$the_files="*.[fF]";
$the_files="*.[F]";
foreach $dir (@the_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("cannot ls to $filename \n");
print $file_name."\n";
	&cleanse($file_name);
}
	$the_files="*.incl";
	$file_name="$outdir/"."include.ls";
	$dir="pick/";
	$ls_opt=$the_path.$dir.$the_files;
print $ls_opt."\n";
	system "ls $ls_opt >$file_name " || die ("cannot ls to $filename \n");
print $file_name."\n";

print "DONE\nNow try typing f90tohtml mm5.f2h\n";

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
	else
        {
	    print CLEAN $line;}
	}
        close CLEAN;
}
