#!/usr/bin/perl
use Pithub;
use Data::Dumper;
use File::Slurp;
my $token = read_file(".ghtoken");
chomp($token);
my $p = Pithub->new('token' => $token, 
	user => 'apache',
	repo => 'spark');
my $target = "holdenk";
my $max = 10999;
my $id = 1;

while ($id < $max) {
    print "starting $id\n";
    my $pr_comments = $p->pull_requests->comments->list(
	pull_request_id => $id);
    my $issue_comments = $p->issues->comments->list(
	issue_id => $id);
    my $involved = 0;
    while (my $comment = $issue_comments->next) {
	if ($comment->{"user"}->{"login"} eq $target) {
	    $involved = 1;
	}
    }
    if ($involved) {
	my $pr_info = $p->pull_requests->get(pull_request_id => $id);
	if (!($pr_info->content->{"user"}->{"login"} eq $target)) {
	    print "https://github.com/apache/spark/pull/$id\n";
	} else {
	    print "close $id :(\n";
	}
    } else {
	print "nyet $id\n";
    }
    $id++;
    sleep(1);
}
