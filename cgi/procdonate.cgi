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

my $baseurl = $config{baseurl};
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
my $paytype = "";
my $stripeamount = "";
my $euroamount = "";
my $currency = "";
my $name = "";
my $mail = "";
my $message = "";
my $separef = "";
my $errorstr = "";

# We use a dictionary to track error.  Those errors will then be
# inserted into the output by write_template.
my %errdict = ();

# Prototypes
sub fail ($);
sub get_paypal_approval ();
sub complete_sepa ();


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

    my $errorpanel = $errorstr;
    my $err_amount = '';
    my $err_name = '';
    my $err_mail = '';
    my $err_paytype = '';
    my $check_checked = ' checked="checked"';
    my $sel_eur = '';
    my $sel_usd = '';
    my $sel_gbp = '';
    my $sel_jpy = '';
    my $message_fmt;
    my $publishname;
    my $check_paytype = 'none';

    # Avoid broken HTML attributes.
    $amount =~ s/\x22/\x27/g;
    $stripeamount =~ s/\x22/\x27/g;
    $currency =~ s/\x22/\x27/g;
    $name =~ s/\x22/\x27/g;
    $mail =~ s/\x22/\x27/g;
    $message =~ s/\x22/\x27/g;
    $separef =~ s/\x22/\x27/g;

    # Clean possible user provided data
    $sessid =~ s/</\x26lt;/g;
    $amount =~ s/</\x26lt;/g;
    $stripeamount =~ s/</\x26lt;/g;
    $currency =~ s/</\x26lt;/g;
    $name =~ s/</\x26lt;/g;
    $mail =~ s/</\x26lt;/g;
    $message =~ s/</\x26lt;/g;
    $separef =~ s/</\x26lt;/g;

    # No need to clean $euroamount.

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

    if ( $paytype eq "cc" ) {
        $check_paytype = "CC";
    } elsif ( $paytype eq "pp" ) {
        $check_paytype = "PP";
    } elsif ( $paytype eq "se" ) {
        $check_paytype = "SE";
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
        elsif (/paytype/){ $err_paytype = $error_marker; }

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
            || s/(\x22\x2f>)?<!--EUROAMOUNT-->/$euroamount\1/
            || s/(\x22\x2f>)?<!--STRIPEAMOUNT-->/$stripeamount\1/
            || s/(\x22\x2f>)?<!--CURRENCY-->/$currency\1/
            || s/(\x22\x2f>)?<!--NAME-->/$name\1/
            || s/(\x22\x2f>)?<!--MAIL-->/$mail\1/
            || s/\x2f><!--CHECKOTHER-->/$check_checked\x2f>/
            || s/\x2f><!--CHECK_$check_paytype-->/$check_checked\x2f>/
            || s/(<\x2ftextarea>)?<!--MESSAGE-->/$message\1/
            || s/<!--MESSAGE_FMT-->/$message_fmt/
            || s/(<selected=\x22selected\x22)?><!--SEL_EUR-->/$sel_eur>/
            || s/(<selected=\x22selected\x22)?><!--SEL_USD-->/$sel_usd>/
            || s/(<selected=\x22selected\x22)?><!--SEL_GBP-->/$sel_gbp>/
            || s/(<selected=\x22selected\x22)?><!--SEL_JPY-->/$sel_jpy>/
            || s/<!--PUBLISH_NAME-->/$publishname/
            || s/<!--SEPA_REF-->/$separef/
            || s/<!--ERRORSTR-->/$errorstr/
            || s/<!--ERR_AMOUNT-->/$err_amount/
            || s/<!--ERR_NAME-->/$err_name/
            || s/<!--ERR_MAIL-->/$err_mail/
            || s/<!--ERR_PAYTYPE-->/$err_paytype/
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
        or fail "Error connecting to payprocd: $!";
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


# Write a dummy page
sub write_overload_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    $errorstr =
        '<p>The system is currently processing too many requests.</p>'
        . '<p>Please retry later.</p>';

    &write_template("donate/error.html");
}

sub write_cancel_page ()
{
    print $q->header(-type=>'text/html', -charset=>'utf-8');
    print "\n";
    &write_template("donate/paypal-can.html");
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
    if ( $paytype eq "cc" ) {
        write_template("donate/checkout-cc.html");
    }
    elsif ( $paytype eq "pp" ) {
        write_template("donate/checkout-pp.html");
    }
    else {
        # For SEPA this is the final page
        write_template("donate/checkout-se.html");
    }
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
    my %sepa;
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
    $euroamount = $data{"Euro"};

    # Check that at least some Euros are given.  Due to Stripe
    # processing fees and our own costs for bookkeeping we need to ask
    # for a minimum amount.
    if ( (not $anyerr) and ($euroamount < 4.00) ) {
        $errdict{"amount"} = 'Sorry, due to overhead costs we do' .
                             ' not accept donations of less than 4 Euro.';
        $anyerr = 1;
    }

    # Check the payment type
    $paytype = $q->param("paytype");
    if ( $paytype ne "cc" and $paytype ne "pp" and $paytype ne "se" ) {
        $errdict{"paytype"} = 'No payment type selected.' .
                              ' Use "Credit Card", "PayPal", or "SEPA".';
        $anyerr = 1;
    }

    # SEPA credit transfers are only possible in Euro.
    # (yes, this may overwrite an earlier error message).
    if ( $paytype eq "se" and $currency ne "EUR" ) {
        $errdict{"amount"} = 'SEPA transfers are only possible in EUR.';
        $anyerr = 1;
    }

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
    $data{"Euroamount"} = $euroamount;
    $data{"Name"} = $name;
    $data{"Mail"} = $mail;
    $data{"Message"} = $message;
    $data{"Paytype"} = $paytype;
    payproc ('SESSION create', \%data ) or fail $data{"ERR_Description"};
    $sessid = $data{"_SESSID"};

    # Send the checkout page or redirect to paypal
    if ( $paytype eq "pp" ) {
        get_paypal_approval ();
    }
    elsif ( $paytype eq "se" ) {
        complete_sepa ();
    }
    else {
        write_checkout_page();
    }
}

# This simply resends the main page again.
sub resend_main_page ()
{
    my %data;

    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};
    $paytype = $data{"Paytype"};
    $stripeamount = $data{"Stripeamount"};
    $euroamount = $data{"Euroamount"};
    $name = $data{"Name"};
    $mail = $data{"Mail"};
    $message = $data{"Message"};

    write_main_page();
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
        write_checkout_page ();
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


# Initiate a payment with paypal and redirect to the Paypal site.
sub get_paypal_approval ()
{
    my %data;
    my %request;
    my $redirurl;

    payproc ('SESSION get ' . $sessid, \%data)
        or fail $data{"ERR_Description"};

    $request{"Currency"} = $data{"Currency"};
    $request{"Amount"} = $data{"Amount"};
    $request{"Desc"} =
        "Donation of " . $data{"Amount"} . " " . $data{"Currency"} .
        " to the GnuPG project";
    $request{"Meta[name]"} = $data{"Name"} unless
        $data{"Name"} eq 'Anonymous';
    $request{"Meta[mail]"} = $data{"Mail"};
    if ($data{"Message"} ne '') {
        $request{"Meta[message]"} = $data{"Message"};
    }
    $request{"Return-Url"} =
        $baseurl . "/cgi-bin/procdonate.cgi?mode=confirm-paypal";
    $request{"Cancel-Url"} =
        $baseurl . "/cgi-bin/procdonate.cgi?mode=cancel-paypal";
    $request{"Session-Id"} = $sessid;

    if (payproc ('GETINFO live', ())) {
      $request{"Paypal-Xp"} = "XP-HD8G-XZRE-W7MH-EYNF";
    } else {
      $request{"Paypal-Xp"} = "XP-NBWZ-QR6Z-8CXV-Q8XS";
    }

    if (not payproc ('PPCHECKOUT prepare', \%request)) {
        $errorstr = $request{"ERR_Description"};
        # Back to the main page.
        write_main_page();
        return;
    }

    $redirurl = $request{"Redirect-Url"};

    #print STDERR "Redirecting to: $redirurl\n";
    print $q->redirect($redirurl) unless $redirurl eq "";
}


# The is called by paypal after approval.  We need to extract the alias
# and the payerid and store it in the session.  Then we ask to confirm
# the payment.
sub confirm_paypal_checkout ()
{
    my $aliasid;
    my $payerid;
    my %data;

    $aliasid = $q->param("aliasid");
    $payerid = $q->param("PayerID");

    # Get the session from the alias and store the aliasid and the
    # payerid in the session.
    payproc ('SESSION sessid ' . $aliasid, \%data)
        or fail $data{"ERR_Description"};
    $sessid = $data{"_SESSID"};
    payproc ('SESSION get ' . $sessid, \%data)
        or fail $data{"ERR_Description"};

    if ( $data{"Paytype"} ne "pp" ) {
        fail "Invalid paytype for Paypal transaction";
    }

    # Put a description for the thanks page into the session data.
    # We do this only now because we send a reduced Desc field to paypal.
    $data{"Desc"} =
        "GnuPG donation by " . $data{"Name"} . " <" . $data{"Mail"} . ">";

    # Note that the capitalization of session data names must match
    # the rules of payprocd.
    $data{"Paypal_aliasid"} = $aliasid;
    $data{"Paypal_payerid"} = $payerid;

    # Set vars for the checkout page.
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};
    $paytype = $data{"Paytype"};
    $stripeamount = $data{"Stripeamount"};
    $euroamount = $data{"Euroamount"};
    $name = $data{"Name"};
    $mail = $data{"Mail"};
    $message = $data{"Message"};

    # Store the session after setting the above vars because that call
    # clears DATA.
    payproc ('SESSION put ' . $sessid, \%data)
        or fail $data{"ERR_Description"};

    # Write the checkout (i.e. confirm payment) page
    write_checkout_page ();
}


# The approved Paypal payment has been approved.  Now execute the
# payment.
sub complete_paypal_checkout ()
{
    my %data;
    my %request;

    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};

    $request{"Alias-Id"}     = $data{"Paypal_aliasid"};
    $request{"Paypal-Payer"} = $data{"Paypal_payerid"};

    if (not payproc ('PPCHECKOUT execute', \%request)) {
        $errorstr =
            '<p>Error: ' . $request{"failure"} . '</p><p>'
            . $request{"failure-mesg"} . '</p>';

        print $q->header(-type=>'text/html', -charset=>'utf-8');
        print "\n";
        write_template("donate/error.html");
        return;
    }

    # Print thanks

    $message = <<EOF;
Amount ..: $request{"Amount"} $request{"Currency"}
Desc ....: $data{"Desc"}
Cardno...: n/a
Processor: PayPal
Email ...: $request{"Email"}
Charge-Id: $request{"Charge-Id"}
Timestamp: $request{"_timestamp"}
EOF
    if ($request{"Live"} eq 'f') {
        $message = $message . "\n!!! TEST TRANSACTION !!!";
    }

    write_thanks_page ();
    payproc ('SESSION destroy ' . $sessid, ());
}


# Complete the SEPA payment: Check values and show final page.
sub complete_sepa ()
{
    my %data;
    my %request;

    payproc ('SESSION get ' . $sessid, \%data)
        or fail $data{"ERR_Description"};

    $request{"Currency"} = $data{"Currency"};
    $request{"Amount"} = $data{"Amount"};
    $request{"Desc"} = "GnuPG SEPA donation";
    $request{"Meta[name]"} = $data{"Name"} unless $data{"Name"} eq 'Anonymous';
    if ($data{"Mail"} ne '') {
        $request{"Meta[mail]"} = $data{"Mail"};
    }
    if ($data{"Message"} ne '') {
        $request{"Meta[message]"} = $data{"Message"};
    }
    if (not payproc ('SEPAPREORDER', \%request )) {
        $errorstr = "Error: " . $request{"ERR_Description"};
        # Back to the main page.
        write_main_page ();
        return;
    }
    $separef = $request{"Sepa-Ref"};
    $amount = $request{"Amount"};

    # Set remaining vars for the checkout page.
    $currency = $data{"Currency"};
    $paytype = $data{"Paytype"};
    $stripeamount = $data{"Stripeamount"};
    $euroamount = $data{"Euroamount"};
    $name = $data{"Name"};
    $mail = $data{"Mail"};
    $message = $data{"Message"};

    write_checkout_page ();
}


# Send a PING command to see whether payprocd is alive.
sub ping_pong ()
{
    my %data = ();

    if (payproc ('PING', \%data )) {
       print $q->header(-type=>'text/HTML', -charset=>'utf-8');
       print "\n";
       print "<p>OK</p>\n";
    }
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
elsif ($mode eq '') {
    # No mode: Show empty template.
    write_main_page();
}
elsif ($mode eq 'ping') {
    # Check aliveness
    ping_pong();
}
elsif ($mode eq 'main') {
    # Returning from the donation start page
    check_donation();
}
elsif ($mode eq 're-main') {
    # Returning from the donation start page
    resend_main_page();
}
elsif ($mode eq 'checkout-stripe') {
    # we have the stripe token - charge the card.
    complete_stripe_checkout();
}
elsif ($mode eq 'cancel-paypal') {
    # Fixme: Destroy the alias of the session.
    write_cancel_page();
}
elsif ($mode eq 'confirm-paypal') {
    # We have approval from Paypal - show the confirm checkout page.
    confirm_paypal_checkout();
}
elsif ($mode eq 'checkout-paypal') {
    # The approved Paypal payment has been approved - charge.
    complete_paypal_checkout();
}
elsif ($mode eq 'pong') {
    # Helper to test a script checking PING.
    fail "Error connecting to payprocd: Forced to fail";
}
else {
    fail('Internal error: Unknown mode');
}
