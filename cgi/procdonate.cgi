#!/usr/bin/perl -T

# procdonate.cgi - Donation payment processor for gnupg.org
# Copyright (C) 2014 g10 Code GmbH
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


use strict;
use CGI;
use Cwd qw(realpath);
use IO::Socket::UNIX;

realpath($0) =~ /^(.*)\/.*$/;
my %config = do $1 . '/config.rc';

my $htdocs =  $config{htdocs};
my $socket_name = $config{payprocd_socket};
my $error_marker = '<span style="color: red;">* error</span>';

# The form variabales are accessed via Q.
my $q  = new CGI;

# This is a multi-purpose CGI.  The mode decides what to do.
my $mode = $q->param("mode");
my $sessid = $q->param("sessid");

# Variables used in the template pages.
my $amount = "";
my $stripeamount = "";
my $currency = "";
my $name = "";
my $mail = "";
my $message = "";
my $errorstr = "";

# We use a dictionary to track error.  Those errors will then be
# inserted into the output by write_template.
my %errdict = ();

# Prototypes
sub fail ($);


# Write a template file.  A template is a proper HTML file with
# variables enclosed in HTML comments.  To allow inserting data into
# a value attribute of an input field, such a tag needs to be written as
#   <input value=""/><!--FOO-->
# the result after processing will be
#   <input value="foo"/>
# assuming that the value of FOO is foo. Note that this substitution
# rules work for all tags and thus you better take care to add an
# extra space if if do not want this to happen.
sub write_template ($) {
    my $fname = shift;

    my $errorpanel = '';
    my $err_amount = '';
    my $err_name = '';
    my $err_mail = '';
    my $checkother = ' checked="checked"';
    my $sel_eur = '';
    my $sel_usd = '';
    my $sel_gbp = '';
    my $sel_jpy = '';
    my $message_fmt;
    my $publishname;

    # Avoid broken HTML attributes.
    $amount =~ s/\x22/\x27/g;
    $stripeamount =~ s/\x22/\x27/g;
    $currency =~ s/\x22/\x27/g;
    $name =~ s/\x22/\x27/g;
    $mail =~ s/\x22/\x27/g;
    $message =~ s/\x22/\x27/g;

    # Clean possible user provided data
    $sessid =~ s/</\x26lt;/g;
    $amount =~ s/</\x26lt;/g;
    $stripeamount =~ s/</\x26lt;/g;
    $currency =~ s/</\x26lt;/g;
    $name =~ s/</\x26lt;/g;
    $mail =~ s/</\x26lt;/g;
    $message =~ s/</\x26lt;/g;

    # Create a formatted message.
    $message_fmt = $message;
    $message_fmt =~ s/\n/<br\x2f>/g;

    if ( $currency =~ /EUR/i ) {
        $sel_eur = ' selected="selected"';
    } elsif ( $currency =~ /USD/i ) {
        $sel_usd = ' selected="selected"';
    } elsif ( $currency =~ /GBP/i ) {
        $sel_gbp = ' selected="selected"';
    } elsif ( $currency =~ /JPY/i ) {
        $sel_jpy = ' selected="selected"';
    }

    # Set var for the paypal button
    if ( $name eq 'Anonymous' or $name eq '') {
        $publishname = 'No';
    } else {
        $publishname = 'Yes';
    }

    # Build error strings.
    foreach (keys %errdict)
    {
        if    (/amount/) { $err_amount = $error_marker; }
        elsif (/name/)   { $err_name   = $error_marker; }
        elsif (/mail/)   { $err_mail   = $error_marker; }

        $errorpanel = $errorpanel . "Field $_: " . $errdict{$_} . "<br/>\n"
    }
    if ( $errorpanel ne '' )
    {
        $errorpanel =
            "<div style='color: red;'><p>\n" . $errorpanel . "</p></div>\n";
    }

    open TEMPLATE, $htdocs . $fname;
    while (<TEMPLATE>) {
        if ( /<!--/ )
        {
            # Only one replacement per line allowed to avoid recursive
            # replacements. Note that MESSAGE uses a special treatment
            # for the textarea tag.
            s/<!--SESSID-->/$sessid/
            || s/(\x22\x2f>)?<!--AMOUNT-->/$amount\1/
            || s/(\x22\x2f>)?<!--STRIPEAMOUNT-->/$stripeamount\1/
            || s/(\x22\x2f>)?<!--CURRENCY-->/$currency\1/
            || s/(\x22\x2f>)?<!--NAME-->/$name\1/
            || s/(\x22\x2f>)?<!--MAIL-->/$mail\1/
            || s/\x2f><!--CHECKOTHER-->/$checkother\x2f>/
            || s/(<\x2ftextarea>)?<!--MESSAGE-->/$message\1/
            || s/<!--MESSAGE_FMT-->/$message_fmt/
            || s/(<selected=\x22selected\x22)?><!--SEL_EUR-->/$sel_eur>/
            || s/(<selected=\x22selected\x22)?><!--SEL_USD-->/$sel_usd>/
            || s/(<selected=\x22selected\x22)?><!--SEL_GBP-->/$sel_gbp>/
            || s/(<selected=\x22selected\x22)?><!--SEL_JPY-->/$sel_jpy>/
            || s/<!--PUBLISH_NAME-->/$publishname/
            || s/<!--ERRORSTR-->/$errorstr/
            || s/<!--ERR_AMOUNT-->/$err_amount/
            || s/<!--ERR_NAME-->/$err_name/
            || s/<!--ERR_MAIL-->/$err_mail/
            || s/<!--ERRORPANEL-->/$errorpanel/;
        }
        print;
    }
    close TEMPLATE;
    $errorstr = "";
}


# Call the payment processor daemon.  Takes the command and a
# reference to a dictionary with the data as input.  On return that
# disctionary is replaced by the response data.
sub payproc ($$)
{
    my $cmd = shift;
    my $data = shift;
    my $sock;
    my $key;
    my $value;
    my $status;
    my $rest;

    # print STDERR "calling payproc: ", $cmd, "<-\n";

    $sock = IO::Socket::UNIX->new($socket_name)
        or fail "socket: $!";
    $sock->print ($cmd, "\n");

    while (($key,$value) = each %$data) {
        next if $key =~ /^_/;
        $value =~ s/\n/\n /g;
        $sock->print ("$key: $value\n");
        # print STDERR "  $key: $value\n";
    }
    $sock->print ("\n");
    $sock->flush or fail "write socket: $!";

    %$data = ();
    while (defined (my $line = <$sock>))
    {
        next if $line =~ /^\#/;
        chomp $line;
        last if $line eq '';
        if (not defined $status)
        {
            ($status, $rest) = split(' ', $line, 2);
            if ( $status eq 'ERR' )
            {
                $rest =~ /\d+\s+\((.*)\).*/;
                $$data{"ERR_Description"} = $1;
            }
        }
        elsif ( $line =~ /^\s+/ )
        {
            fail "bad dict line received" if not defined $key;
            $$data{$key} .= "\n" . substr($line, 1);
        }
        else
        {
            ($key, $value) = split(':', $line, 2);
            $value =~ s/^\s+//;
            $$data{$key} = $value;
        }
    }

    #print STDERR "payproc status: $status (", $$data{"ERR_Description"}, ")\n";
    #while (($key,$value) = each %$data) {
    #     print STDERR "  ", $key, ": ", $value, "\n";
    #}

    $sock->close;
    return 1 if $status eq 'OK';
    return 0 if $status eq 'ERR';
    fail 'payproc did not return a proper status code';
}


# Write a page with all the data inserted.
sub write_overload_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    $errorstr =
        '<p>The system is currently processing too many requests.</p>'
        . '<p>Please retry later.</p>';

    &write_template("donate/error.html");
}


# Write an internal error page
sub fail ($)
{
    my $desc = shift;

# FIXME: write the detailed error only to the log.
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    $errorstr =
        '<p>An internal error occured:</p>'
        . "<p>$desc</p>";

    write_template("donate/error.html");
    exit 0;
}


# Write a the initial donation page.  This is usallay done to show
# errors.  The page is intially shown as static page.
sub write_main_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    write_template("donate/index.html");
}


# Write a page with all the data inserted.
sub write_checkout_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    write_template("donate/checkout.html");
}

# Write a page with all the data inserted specific for cards.
sub write_checkout_cc_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    write_template("donate/checkout-cc.html");
}


# Write the final thank you page.
sub write_thanks_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    write_template("donate/donate-thanks.html");
}


# Check the values entered at the donation page.  Return true if
# everything is alright.  On error the donation page is send again.
sub check_donation ()
{
    my %data;
    my $anyerr = 0;

    # Note: When re-displaying the page we always use amount other
    # because that is easier to implement than figuring out which
    # amount and currency was used and check the appropriate radio
    # button.
    $amount = $q->param("amount");
    if ($amount eq 'other') {
      $amount = $q->param("amountother");
      $currency = $q->param("currency");
    } else {
      $currency = 'EUR';
    }
    $name = $q->param("name");
    $name = 'Anonymous' if $name eq '';
    $mail = $q->param("mail");
    $message = $q->param("message");
    $stripeamount = "0";

    # Check the amount.
    $data{"Amount"} = $amount;
    $data{"Currency"} = $currency;
    if (not payproc ('CHECKAMOUNT', \%data )) {
        $errdict{"amount"} = $data{"ERR_Description"};
        $anyerr = 1;
    }
    $stripeamount = $data{"_amount"};
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};

    # Check the mail address
    if ($mail ne '' and $mail !~ /\S+@\S+\.\S+/ ) {
        $errdict{"mail"} = 'invalid mail address';
        $anyerr = 1;
    }

    # If needed present errors and ask again.  */
    if ($anyerr) {
        write_main_page();
        return;
    }


    # Now create a session.
    $data{"Stripeamount"} = $stripeamount;
    $data{"Name"} = $name;
    $data{"Mail"} = $mail;
    $data{"Message"} = $message;
    payproc ('SESSION create', \%data ) or fail $data{"ERR_Description"};
    $sessid = $data{"_SESSID"};

    # Send the checkout page.
    write_checkout_page();
}

# This simply resends the main page again.
sub resend_main_page ()
{
    my %data;

    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};
    $stripeamount = $data{"Stripeamount"};
    $name = $data{"Name"};
    $mail = $data{"Mail"};
    $message = $data{"Message"};

    write_main_page();
}


# This simply resends the checkout options page.
sub resend_card_checkout ()
{
    my %data;

    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};
    $stripeamount = $data{"Stripeamount"};
    $name = $data{"Name"};
    $mail = $data{"Mail"};
    $message = $data{"Message"};

    write_checkout_page();
}



# This simply sends the card specific checkout page.
sub prepare_card_checkout ()
{
    my %data;

    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};
    $stripeamount = $data{"Stripeamount"};
    $mail = $data{"Mail"};

    write_checkout_cc_page();
}


# This is called by FIXME
sub complete_stripe_checkout ()
{
    my %data;
    my %stripe;

    # fixme: Change the error message to note that the card has not
    # been charged.  Somehow delete the token
    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};

    # Do the checkout.
    $stripe{"Card-Token"} = $q->param("stripeToken");
    $stripe{"Currency"} = $data{"Currency"};
    $stripe{"Amount"} = $data{"Amount"};
    $stripe{"Desc"} =
        "GnuPG donation by " . $data{"Name"} . " <" . $data{"Mail"} . ">";
    $stripe{"Stmt-Desc"} = "GnuPG donation";
    $stripe{"Email"} = $q->param("stripeEmail");
    $stripe{"Meta[name]"} = $data{"Name"} unless $data{"Name"} eq 'Anonymous';
    if ($data{"Mail"} ne $q->param("stripeEmail")) {
        $stripe{"Meta[mail]"} = $data{"Mail"};
    }
    if ($data{"Message"} ne '') {
        $stripe{"Meta[message]"} = $data{"Message"};
    }
    if (not payproc ('CHARGECARD', \%stripe)) {
        $errorstr =
            '<p>Error: ' . $stripe{"failure"} . '</p><p>'
            . $stripe{"failure-mesg"} . '</p>';
        # Again.
        prepare_card_checkout ();
        return;
    }

    # Print thanks

    $message = <<EOF;
Amount ..: $stripe{"Amount"} $stripe{"Currency"}
Desc ....: $stripe{"Desc"}
Cardno...: *$stripe{"Last4"}
Processor: Stripe
Email ...: $stripe{"Email"}
Charge-Id: $stripe{"Charge-Id"}
Timestamp: $stripe{"_timestamp"}
EOF
    if ($stripe{"Live"} eq 'f') {
        $message = $message . "\n!!! TEST TRANSACTION !!!";
    }

    write_thanks_page ();
    payproc ('SESSION destroy ' . $sessid, ());
}





#
# Main
#
#print STDERR "CGI called with mode=$mode\n";
#print STDERR "CGI called with sessid=$sessid\n";
if ($q->param('url') ne '') {
    # If the URL field has been filled out, the client did not follow
    # the instructions and thus failed the Turing test.  Provide an
    # innocent error page.
    write_overload_page ()
}
elsif ($mode eq 'main') {
    # Returning from the donation start page
    check_donation();
}
elsif ($mode eq 're-main') {
    # Returning from the donation start page
    resend_main_page();
}
elsif ($mode eq 're-checkout') {
    # Redisplay the checkout option page
    resend_card_checkout();
}
elsif ($mode eq 'checkout-cc') {
    # The checkout page requested a card checkout.
    prepare_card_checkout();
}
elsif ($mode eq 'checkout-stripe') {
    # we have the stripe token - charge the card.
    complete_stripe_checkout();
}
else {
    fail('Internal error: Unknown mode');
}
