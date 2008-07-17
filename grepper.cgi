#!/usr/bin/perl -w
# put this in your personal cgi-bin 
# the browser directory should be at the same level as the cgi-bin 
use CGI qw(param);
$mmax=500;
$m=0;

my $match=param("grep");
my $glob=param("path");
my $ig=param("case");
my $address=param("address");
my $filelist=param("filelist");

$match=~s/^\s*//;
$match=~s/\s*$//;
$glob=~s/^\s*//;
$glob=~s/\s*$//;
$glob=~s+\.\.++g;
$glob=~s+^/++;
$glob or $glob='*/*';

#$address=param("address");

$ignore='<br>ignore case' if $ig eq 'on';

$tim = `date +%d-%m-%Y/%I.%M%p`;
chomp($tim);
open SEARCH,">>searches";
print SEARCH "$ENV{REMOTE_HOST}  $tim  $address $match   $glob\n";
#die unless $address=~m/^\.\.\/\w/;
#die if $address=~m/.+\.\./;
$where=$address.$glob.'.html';

@sf=glob($where); 

print <<END_of_Start;
Content-type: text/html

<HTML>
    <HEAD>
    <TITLE>f90tohtml search results</TITLE>
    </HEAD>
    <BODY>
    <H1>search results:</H1>
	search for pattern:<br>
	<font color=#009900>$match</font>$ignore<p>
	in:
	<font color=#009900>$glob</font>$len<p>
	<pre><font size=-1>
END_of_Start

$match=~s!\\s!((<.*>)*\\s(<.*>)*)!g;
$match=~s! !((<.*>)* (<.*>)*)!g;
$match=~s!\^!\^(<.*>)*!g;
$abort=0;
$nf=0;
if ($ig eq 'on'){
$command='$line=~m/$match/i';
$command2='$line=~s+($match)+<font color=#ff0000>$1</font>+gi';
$command3='@hits=grep /$match/i, @all';
}
else{
$command='$line=~m/$match/';
$command2='$line=~s+($match)+<font color=#ff0000>$1</font>+g';
$command3='@hits=grep /$match/, @all';
}
for $file (@sf){
	last if $abort;
	$nf++;
	$short=$file;
	$short=~s/^.*html_code\///;
	$short=~s/\.html//;
	$files.=$short."<br>\n";
	open IN,"<$file";
	@all=<IN>;
	eval($command3);
	for $line (@hits){
	chomp($line);
	$line=~m/<a name='(.*)'>$/;
	$ln=$1;
	$line=~s/<.*?>//g;
	if (eval($command)) {
		$m++;
		$line=~s/^\s*//;
		$line=~s/\s*$//;
		$xline=$line;
		eval($command2);		
		$line=~s/\s{2,}/ /;
		$long=$file.'#'.$ln;
		$out="<a href='$long' target='bottom_target'>$short $ln</a>\n $line<br>\n";
		print $out;
	}
	if ($m>=$mmax){
		$abort=1;
		last;
	}
	}
}
print '</pre></font>';
if (!$abort) {
	print "<p>completed search, found <font color=#009900>$m</font> items<br>";
}
else{
	print "<p>aborted search, found <font color=#009900>$m</font> items (maximum allowed is $mmax)<br>";
}

print "<p>searched in  <font color=#009900>$nf</font> files:<p>";
print $files if $filelist eq 'on';
print <<All_Done; 
</body>
</html>
All_Done
