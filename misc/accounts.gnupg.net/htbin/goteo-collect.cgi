#!/usr/bin/perl -T

# goteo-collect.cgi   - Collect data for rewards of the 2013 Goteo campaign
# Copyright (C) 2014 g10 Code GmbH
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This scripts presents a customized page to collect information for
# the Goteo campaign rewards.  it expects a template file
# goteo-collect.html.in with certain '@' delimited variables and data
# file /var/log/gnupg.net/goteo-collect.input with the information to
# be presented to the user.  The result will be logged to the file
# /var/log/gnupg.net/goteo-collect in a mail header like format to be
# later processed using GNU recutils or addrutil.  The input is
# expected to be colon separated with one record per line.  All values
# use percent encoding but may use unencoded spaces.  The fields are:
#
# 1. id      := A random string to identify the record.  This is
#               part of the URL send by mail to the respective
#               contributor.
# 2. account := The Goteo account name
# 3. name    := Name of the contributor
# 4. reward  := Type of the reward: 't' = t-shirt
#                                   'm' = mail address
#                                   's' = sticker
# 5. mail    := real mail address
# 6. address := The address data (only required for 't')

#
# Note that this script needs libmime-base32-perl.

# Preparing the inpout data.  We received the Input data as
# spreadshit. The follwoing steps have been used to create the input
# data:
#  - Use gnumeric to create a CSV file.
#  - Run addrutil like this
#
#     tail -n+2 FILE \
#       ./addrutil -FGoteo-ID -FUser -FName -FMail -FAmount -FProblem \
#                  -FAnon -FReward -FAddress -FDate --readcsv   \
#       | sed '/^Address:/ s/ , , ,//' | sed '/^Address:/ s/,/\n/g' \
#  | awk '/^Date:/{split($2,a,"/");print $1" "a[3]"-"a[2]"-"a[1];next};{print}'
#
#    Now we have the data in an easy to read format.  The tool addrutil is
#    available at 'http://git.gnupg.org/cgi-bin/gitweb.cgi\
#                  ?p=wk-misc.git;f=addrutil.c;a=blob_plain'
#
#  - Change some more strings:
#       cat data \
#       | sed 's/^Reward: An @GnuPG.*/Reward: m/' \
#       | sed 's/^Reward: .*sticker.*/Reward: s/' \
#       | sed 's/^Reward: .*tshirt.*/Reward: t/'  \
#       | sed 's/^Reward: .*Listed.*/Reward: l/' > newdata
#
#  - To add a unique ID use this command
#
#    awk '/^Goteo-ID:/ {cmd="gpg -a --gen-random 0 15
#         | tr +/ 42"; cmd | getline foo; print "Id: " foo; close(cmd); };
#        {print}' data >newdata
#
#  - To finally create the input data
#    fields="-FId -FUser -FName -FReward -FMail -FAddress"
#     ( ./addrutil -SReward=t $fields data \
#      && ./addrutil -SReward=m $fields data \
#      && ./addrutil -SReward=s $fields data ) > goteo-collect.input
#



use CGI;
use POSIX qw(strftime);
use Fcntl qw(:flock SEEK_END);
use MIME::Base32 qw( RFC );

my $time = strftime "%Y-%m-%d %H:%M:%S", gmtime;
my $htdocs = '/var/www/all/accounts.gnupg.net/htdocs/';

my $q  = new CGI;

my $id = $q->param("id");
my $mode = $q->param("mode");
my $address = "";
my $txid = "";
my $errorstr = "";
my $tsize= "";

sub get_txid {
    local $data;

    open (DEVRAND, "<", "/dev/urandom");
    read (DEVRAND, $data, 6);
    close (DEVRAND);
    MIME::Base32::encode($data);
}

# Write the collected data out to the log file.  On return
# $txid has the transaction id.
sub write_logfile {
    open(LOGFILE, ">>", "/var/log/gnupg.net/goteo-collect")
        || die "error opening log file: $!";
    flock(LOGFILE, LOCK_EX) || die "error locking log file: $!";
    seek(LOGFILE, 0, SEEK_END) || die "error seek to end of log file: $!";

    $txid = &get_txid();

    print LOGFILE "Start: $time\n";
    print LOGFILE "txid: $txid\n";
    print LOGFILE "reward: $reward\n";
    foreach $name ($q->param) {
        $value = $q->param($name);
        if ($name !~ /^Start/i) {
            $value =~ s/\r//g;
            chomp $value;
            $value =~ s/\n/\n  /g;
            print LOGFILE "$name: $value\n";
        }
    }
    print LOGFILE "\n";

    flock(LOGFILE, LOCK_UN) || die "error unlocking log file: $!";
    close(LOGFILE) || die "error closing log file: $!";
}


# Find the item for $id and fill in all numbers.  Return false if not
# found.
sub find_item {
    local $tmpid;
    open(INPUTFILE, "<", "/var/log/gnupg.net/goteo-collect.input")
        || die "error opening input file: $!";
    while (<INPUTFILE>) {
        chomp;
        ($tmpid, $account, $name, $reward, $mail, $address) = split (/:/);
        if ($tmpid eq $id) {
            close(INPUTFILE);
            $account =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/ge;
            $name =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/ge;
            $mail =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/ge;
            $address =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/ge;
            return 1;
        }
    }
    close(INPUTFILE);
    0;
}


sub write_template {
    local $fname = shift;
    local $indel = 0;
    local $fulladdress;
    local $tsizehuman;

    if ( "$address" =~ /^$name/ ) {
        $fulladdress = $address;
    } else {
        $fulladdress = "$name\n$address";
    }

    if ( "$tsize" eq 'X' ) {
        $tsizehuman = "XL";
    } elsif ( "$tsize" eq 'Z' ) {
        $tsizehuman = "XXL";
    }
    else {
        $tsizehuman = $tsize;
    }

    open TEMPLATE, $htdocs . $fname;
    while (<TEMPLATE>) {
        if ($indel) {
            $indel = 0 if (/<!--END_/);
            next;
        }
        elsif (($reward eq 'm') && /^<!--BEGIN_(T_SHIRT|STICKER)/) {
            $indel = 1;
            next;
        }
        elsif (($reward eq 't') && /^<!--BEGIN_(MAIL|STICKER)/) {
            $indel = 1;
            next;
        }
        elsif (($reward eq 's') && /^<!--BEGIN_(T_SHIRT|MAIL)/) {
            $indel = 1;
            next;
        }

        # Only one replacement allowed to avoid recursive replacements
        s/\x40ID\x40/$id/g
        || s/\x40NAME\x40/$name/g
        || s/\x40ACCOUNT\x40/$account/g
        || s/\x40ADDRESS\x40/$fulladdress/g
        || s/\x40MAIL\x40/$mail/g
        || s/\x40REWARD\x40/$reward/g
        || s/\x40TXID\x40/$txid/g
        || s/\x40TSIZE\x40/$tsizehuman/g
        || s/\x40MAILBOX\x40/$mailbox/g
        || s/\x40MAILBOX2\x40/$mailbox2/g
        || s/\x40ERRORSTR\x40/$errorstr/g;

        print;
    }
    close TEMPLATE;
}


# Write a page with all the data for the goteo account.
sub write_main_page {
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    &write_template("goteo-collect.html.in");
}

sub write_error_page {
    local $text = shift;

    $errorstr = '<p style="color: red;">' . $text . '</p>';
    # Keep the entered values.
    $mail = $q->param('mail');
    $address = $q->param('address');
    $mailbox = $q->param('mailbox');
    $mailbox2 = $q->param('mailbox2');
    &write_main_page();
}

# Write a page with the collected data
sub write_thanks_page {
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    $mail = $q->param('mail');
    $address = $q->param('address');
    $mailbox = $q->param('mailbox');
    $mailbox2 = $q->param('mailbox2');
    $tsize    = $q->param('tsize');
    &write_template("goteo-collect-thx.html.in");
}


#
# Main
#
if ($id eq '') {
    print $q->header('text/html');
    print $q->start_html('Error');
    print $q->h1('Bad request - please use the correct URL');
    print $q->end_html;
}
elsif (not &find_item()) {
    print $q->header('text/html');
    print $q->start_html('Error');
    print $q->h1('No such URL');
    print $q->p('Please verify that you used the URL from the mail');
    print $q->end_html;
}
elsif ($mode eq 'response') {
    if ($q->param('mail') !~ /[a-zA-Z0-9.-_]+@[a-zA-Z0-9]+/ ) {
        write_error_page('Invalid mail address given!');
    }
    elsif ($reward eq 't' and $q->param('tsize') !~ /^[SMLXZ]$/ ) {
        write_error_page('Size of t-shirt not given!');
    }
    else {
        write_logfile();
        write_thanks_page();
    }
}
else {
    &write_main_page();
}
