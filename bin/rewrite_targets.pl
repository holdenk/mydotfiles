#!/usr/bin/perl
use File::Slurp;

while (my $file = <>) {
    chomp($file);
    my $text = read_file($file);
    write_file($file + ".old", $text);
    $text =~ s/git:\/\/(.*?)\//git@$1:/;
    write_file($file, $text);
}
