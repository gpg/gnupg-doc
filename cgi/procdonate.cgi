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
#use CGI qw/:standard -debug/;
use CGI;
use Cwd qw(realpath);
use IO::Socket::UNIX;

realpath($0) =~ /^(.*)\/.*$/;
my %config = do $1 . '/config.rc';

my $baseurl = $config{baseurl};
my $htdocs =  $config{htdocs};
my $stripepubkey =  $config{stripepubkey};
my $socket_name = $config{payprocd_socket};
my $error_marker = '<span style="color: red;">* error</span>';

# The form variables are accessed via Q.
my $q  = new CGI;

# This is a multi-purpose CGI.  The mode decides what to do.
my $mode = $q->param("mode");
my $sessid = $q->param("sessid");
my $lang = $q->param("lang");

# Variables used in the template pages.
my $amount = "";
my $paytype = "";
my $stripeamount = "";
my $euroamount = "";
my $currency = "";
my $recur = "";
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
# extra space if you do not want this to happen.
sub write_template ($) {
    my $fname = shift;

    my $tname;
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
    my $chk_amt500 = '';
    my $chk_amt200 = '';
    my $chk_amt100 = '';
    my $chk_amt50 = '';
    my $chk_amt20 = '';
    my $chk_amt10 = '';
    my $chk_amt5 = '';
    my $chk_amtx = '';
    my $amt_other = '';
    my $recur_none = '';
    my $recur_month = '';
    my $recur_quarter = '';
    my $recur_year = '';
    my $recur_text = '';
    my $message_fmt;
    my $publishname;
    my $check_paytype = 'none';
    my $stripe_data_email = '';
    my $stripe_data_label_value;
    my $xamount;
    my $stripelocale;

    # Avoid broken HTML attributes.
    $amount =~ s/\x22/\x27/g;
    $stripeamount =~ s/\x22/\x27/g;
    $currency =~ s/\x22/\x27/g;
    $recur =~ s/\x22/\x27/g;
    $name =~ s/\x22/\x27/g;
    $mail =~ s/\x22/\x27/g;
    $message =~ s/\x22/\x27/g;
    $separef =~ s/\x22/\x27/g;
    $lang =~ s/\x22/\x27/g;

    # Clean possible user provided data
    $sessid =~ s/</\x26lt;/g;
    $lang =~ s/</\x26lt;/g;
    $amount =~ s/</\x26lt;/g;
    $stripeamount =~ s/</\x26lt;/g;
    $currency =~ s/</\x26lt;/g;
    $recur =~ s/</\x26lt;/g;
    $name =~ s/</\x26lt;/g;
    $mail =~ s/</\x26lt;/g;
    $message =~ s/</\x26lt;/g;
    $separef =~ s/</\x26lt;/g;

    # No need to clean $euroamount.

    # Check whether a translated template is available.
    $tname = $htdocs . $fname;
    $tname =~ s/\.html$/.$lang.html/;
    if ( not -f $tname ) { $tname = $htdocs . $fname; }

    # Create a formatted message.
    $message_fmt = $message;
    $message_fmt =~ s/\n/<br\x2f>/g;

    # Check the currency and predefined amount.
    if ( $currency =~ /EUR/i ) {
        $sel_eur = ' selected="selected"';
        $xamount = int $amount;
        if ( $xamount == 5 ) {
            $chk_amt5 = $check_checked;
        } elsif ( $xamount == 10 ) {
            $chk_amt10 = $check_checked;
        } elsif ( $xamount = 20 ) {
            $chk_amt20 = $check_checked;
        } elsif ( $xamount == 50 ) {
            $chk_amt50 = $check_checked;
        } elsif ( $xamount == 100 ) {
            $chk_amt100 = $check_checked;
        } elsif ( $xamount == 200 ) {
            $chk_amt200 = $check_checked;
        } elsif ( $xamount == 500 ) {
            $chk_amt500 = $check_checked;
        } else {
            $chk_amtx = $check_checked;
            $amt_other = $amount;
        }
    } elsif ( $currency =~ /USD/i ) {
        $sel_usd = ' selected="selected"';
        $chk_amtx = $check_checked;
        $amt_other = $amount;
    } elsif ( $currency =~ /GBP/i ) {
        $sel_gbp = ' selected="selected"';
        $chk_amtx = $check_checked;
        $amt_other = $amount;
    } elsif ( $currency =~ /JPY/i ) {
        $sel_jpy = ' selected="selected"';
        $chk_amtx = $check_checked;
        $amt_other = $amount;
    } else {
        $chk_amtx = $check_checked;
        $amt_other = $amount;
    }

    # For non-recurring Stripe donations we do not want to send a
    #     data-email="$mail"
    # line to Stripe so to enable the user to use a a different mail
    # address for use with them.  This is implemented using a
    # STRIPE_DATA_EMAIL template variable.
    $stripe_data_email = 'data-email="' . $mail . '"';
    if ( $recur =~ /0/ ) {
        $stripe_data_email = '';
        $recur_none    = ' selected="selected"';
        $recur_text    = '';

        if ($lang eq 'de') {
            $stripe_data_label_value = 'Einmalig spenden';
        } elsif ($lang eq 'ja') {
            $stripe_data_label_value = '一回の寄付する';
        } else {
            $stripe_data_label_value = 'Make one-time donation';
        }

    } elsif ( $recur =~ /12/ ) {
        $recur_month   = ' selected="selected"';

        if ($lang eq 'de') {
            $recur_text    = 'monatlich';
            $stripe_data_label_value = 'Monatlich spenden';
        } elsif ($lang eq 'ja') {
            $recur_text    = '毎月';
            $stripe_data_label_value = '毎月寄付する';
        } else {
            $recur_text    = 'monthly';
            $stripe_data_label_value = 'Donate monthly';
        }

    } elsif ( $recur =~ /4/ ) {
        $recur_quarter = ' selected="selected"';

        if ($lang eq 'de') {
            $recur_text    = 'vierteljährlich';
            $stripe_data_label_value = 'Vierteljährlich spenden';
        } elsif ($lang eq 'ja') {
            $recur_text    = '3ヶ月毎';
            $stripe_data_label_value = '3ヶ月毎に寄付する';
        } else {
            $recur_text    = 'quarterly';
            $stripe_data_label_value = 'Donate quarterly';
        }

    } elsif ( $recur =~ /1/ ) {
        $recur_year    = ' selected="selected"';

        if ($lang eq 'de') {
            $recur_text    = 'jährlich';
            $stripe_data_label_value = 'Jährlich spenden';
        } elsif ($lang eq 'ja') {
            $recur_text    = '毎年';
            $stripe_data_label_value = '毎年寄付する';
        } else {
            $recur_text    = 'yearly';
            $stripe_data_label_value = 'Donate yearly';
        }

    } else { # invalid
        $stripe_data_label_value = '';
    }

    if ( $paytype eq "cc" ) {
        $check_paytype = "CC";
    } elsif ( $paytype eq "pp" ) {
        $check_paytype = "PP";
    } elsif ( $paytype eq "se" ) {
        $check_paytype = "SE";
    } elsif ( $paytype eq "bc" ) {
        $check_paytype = "BC";
    }

    # Set var for the paypal button
    if ( $name eq 'Anonymous' or $name eq '') {
        $publishname = 'No';
    } else {
        $publishname = 'Yes';
    }



    # Set a specific locale.
    if ($lang eq 'de')    { $stripelocale = "de"; }
    elsif ($lang eq 'ja') { $stripelocale = "ja"; }
    elsif ($lang eq 'en') { $stripelocale = "en"; }
    else                  { $stripelocale = "auto"; }


    # Build error strings.
    foreach (keys %errdict)
    {
        my $fieldname;

        if ($lang eq 'de')    { $fieldname = "Feld $_: ";  }
        elsif ($lang eq 'ja') { $fieldname = "欄 $_: "; }
        else                  { $fieldname = "Field $_: "; }

        if    (/amount/) { $err_amount = $error_marker; }
        elsif (/name/)   { $err_name   = $error_marker; }
        elsif (/mail/)   { $err_mail   = $error_marker; }
        elsif (/paytype/){ $err_paytype = $error_marker; }

        $errorpanel = $errorpanel . $fieldname . $errdict{$_} . "<br/>\n"
    }
    if ( $errorpanel ne '' )
    {
        $errorpanel =
            "<div style='color: red;'><p>\n" . $errorpanel . "</p></div>\n";
    }


    open TEMPLATE, $tname;
    while (<TEMPLATE>) {
        if ( /<!--/ )
        {
        # Only one replacement per line allowed to avoid recursive
        # replacements. Note that MESSAGE uses a special treatment
        # for the textarea tag.
        s/<!--SESSID-->/$sessid/
        || s/(\x22\x2f>)?<!--AMOUNT-->/$amount\1/
        || s/(\x22\x2f>)?<!--AMT_OTHER-->/$amt_other\1/
        || s/(\x22\x2f>)?<!--EUROAMOUNT-->/$euroamount\1/
        || s/(\x22\x2f>)?<!--STRIPEPUBKEY-->/$stripepubkey\1/
        || s/(\x22\x2f>)?<!--STRIPELOCALE-->/$stripelocale\1/
        || s/(\x22\x2f>)?<!--STRIPEAMOUNT-->/$stripeamount\1/
        || s/(\x22\x2f>)?<!--CURRENCY-->/$currency\1/
        || s/(\x22\x2f>)?<!--NAME-->/$name\1/
        || s/(\x22\x2f>)?<!--MAIL-->/$mail\1/
        || s/\x2f><!--CHECK_$check_paytype-->/$check_checked\x2f>/
        || s/(<\x2ftextarea>)?<!--MESSAGE-->/$message\1/
        || s/<!--MESSAGE_FMT-->/$message_fmt/
        || s/(<selected=\x22selected\x22)?><!--SEL_EUR-->/$sel_eur>/
        || s/(<selected=\x22selected\x22)?><!--SEL_USD-->/$sel_usd>/
        || s/(<selected=\x22selected\x22)?><!--SEL_GBP-->/$sel_gbp>/
        || s/(<selected=\x22selected\x22)?><!--SEL_JPY-->/$sel_jpy>/
        || s/(<selected=\x22selected\x22)?><!--RECUR_NONE-->/$recur_none>/
        || s/(<selected=\x22selected\x22)?><!--RECUR_MONTH-->/$recur_month>/
        || s/(<selected=\x22selected\x22)?><!--RECUR_QUARTER-->/$recur_quarter>/
        || s/(<selected=\x22selected\x22)?><!--RECUR_YEAR-->/$recur_year>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT500-->/$chk_amt500\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT200-->/$chk_amt200\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT100-->/$chk_amt100\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT50-->/$chk_amt50\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT20-->/$chk_amt20\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT10-->/$chk_amt10\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMT5-->/$chk_amt5\x2f>/
        || s/(<check=\x22checked\x22)?\x2f><!--CHK_AMTX-->/$chk_amtx\x2f>/
        || s/<!--RECUR_TEXT-->/$recur_text/
        || s/<!--STRIPE_DATA_EMAIL-->/$stripe_data_email/
        || s/<!--STRIPE_DATA_LABEL_VALUE-->/$stripe_data_label_value/
        || s/<!--PUBLISH_NAME-->/$publishname/
        || s/<!--LANG-->/$lang/
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
    write_template("donate/donate.html");
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
    elsif ( $paytype eq "bc" ) {
        # For Bitcoins this is the final page
        write_template("donate/checkout-bc.html");
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
    my $msg;

    $amount = $q->param("amount");
    if ($amount eq 'other') {
      $amount = $q->param("amountother");
      $currency = $q->param("currency");
    } else {
      $currency = 'EUR';
    }

    $recur = $q->param("recur");
    $name = $q->param("name");
    $name = 'Anonymous' if $name eq '';
    $mail = $q->param("mail");
    $message = $q->param("message");
    $stripeamount = "0";

    # Check the amount and the recurring value
    $data{"Amount"} = $amount;
    $data{"Currency"} = $currency;
    $data{"Recur"} = $recur;
    if (not payproc ('CHECKAMOUNT', \%data )) {
        $errdict{"amount"} = $data{"ERR_Description"};
        $anyerr = 1;
    }
    $stripeamount = $data{"_amount"};
    $amount = $data{"Amount"};
    $recur = $data{"Recur"};
    $currency = $data{"Currency"};
    $euroamount = $data{"Euro"};

    # Check that at least some Euros are given.  Due to Stripe
    # processing fees and our own costs for bookkeeping we need to ask
    # for a minimum amount.
    if ( (not $anyerr) and ($euroamount < 4.00) ) {

        if ($lang eq 'de') {
            $msg= 'Um unsere Verwaltungskosten niedrig zu halten,'
                . 'können wir leider keine Spenden unter 4 Euro annehmen.';
        } elsif ($lang eq 'ja') {
            $msg = '申し訳ありません。間接経費のため、4ユーロ未満の寄付'
                . 'は受け付けることができません。';
        }
        else {
            $msg = 'Sorry, due to overhead costs we do'
                . ' not accept donations of less than 4 Euro.';
        }

        $errdict{"amount"} = $msg;
        $anyerr = 1;
    }

    # Check the payment type
    $paytype = $q->param("paytype");
    if ( $paytype eq "bc" ) {
        # No further checks - this is kind of a hack.
    }
    elsif ( $paytype ne "cc" and $paytype ne "pp" and $paytype ne "se" ) {

        if ($lang eq 'de') {
            $msg= 'Keine Zahlungsart angegeben.'
                . ' Bitte "Kreditkarte", "PayPal" oder "SEPA" auswählen.';
        } elsif ($lang eq 'ja') {
            $msg= '支払い方式が選択されていません。'
                . '"クレジットカード", "PayPal", または "SEPA" が選択できます。';
        }
        else {
            $msg= 'No payment type selected.'
                . ' Use "Credit Card", "PayPal", or "SEPA".';
        }

        $errdict{"paytype"} = $msg;
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
    $data{"lang"} = $lang;
    $data{"Stripeamount"} = $stripeamount;
    $data{"Euroamount"} = $euroamount;
    $data{"Recur"} = $recur;
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
    # If the session has a lang value use that.
    if ($data{"lang"} ne '') {
        $lang = $data{"lang"};
    }
    $amount = $data{"Amount"};
    $currency = $data{"Currency"};
    $recur = $data{"Recur"};
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
    my $recur;
    my $recur_text = '';

    # fixme: Change the error message to note that the card has not
    # been charged.  Somehow delete the token
    payproc ('SESSION get ' . $sessid, \%data) or fail $data{"ERR_Description"};

    # If the session has a lang value use that.
    if ($data{"lang"} ne '') {
        $lang = $data{"lang"};
    }

    # Do the checkout.
    $stripe{"Card-Token"} = $q->param("stripeToken");
    $stripe{"Currency"} = $data{"Currency"};
    $stripe{"Amount"} = $data{"Amount"};
    $stripe{"Desc"} =
        "GnuPG donation by " . $data{"Name"} . " <" . $data{"Mail"} . ">";
    $stripe{"Stmt-Desc"} = "GnuPG donation";
    $stripe{"Email"} = $q->param("stripeEmail");
    $stripe{"Recur"} = $data{"Recur"};
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
    $recur = $stripe{"Recur"};
    if ( $recur =~ /12/ ) {
        if ($lang eq 'de')    { $recur_text = 'monatlich'; }
        elsif ($lang eq 'ja') { $recur_text = '毎月'; }
        else                  { $recur_text = 'Monthly'; }
    } elsif ( $recur =~ /4/ ) {
        if ($lang eq 'de')    { $recur_text = 'vierteljährlich'; }
        elsif ($lang eq 'ja') { $recur_text = '3ヶ月毎'; }
        else                  { $recur_text = 'Quarterly'; }
    } elsif ( $recur =~ /1/ ) {
        if ($lang eq 'de')    { $recur_text = 'jährlich'; }
        elsif ($lang eq 'ja') { $recur_text = '毎年'; }
        else                  { $recur_text = 'Yearly'; }
    } else {
        if ($lang eq 'de')    { $recur_text = 'nein'; }
        elsif ($lang eq 'ja') { $recur_text = '一回だけ'; }
        else                  { $recur_text = 'Just once'; }
    }

    if ($lang eq 'de') {
        $message = <<EOF;
Betrag ......: $stripe{"Amount"} $stripe{"Currency"}
Dauerauftrag : $recur_text
Beschreibung : $stripe{"Desc"}
Kartennr. ...: *$stripe{"Last4"}
Dienstleister: Stripe
Charge-Id ...: $stripe{"Charge-Id"}
Zeitstempel .: $stripe{"_timestamp"}
Email .......: $stripe{"Email"}
EOF
        if ($stripe{"account-id"} ne '') {
            $message = $message . "Spender-ID ..: " . $stripe{"account-id"};
        }
    } elsif ($lang eq 'ja') {
        $message = <<EOF;
金額 ......: $stripe{"Amount"} $stripe{"Currency"}
毎回?一回? : $recur_text
説明 ......: $stripe{"Desc"}
カード番号 : *$stripe{"Last4"}
決済業者 ..: Stripe
Charge-Id .: $stripe{"Charge-Id"}
時刻 ......: $stripe{"_timestamp"}
メール ....: $stripe{"Email"}
EOF
        if ($stripe{"account-id"} ne '') {
            $message = $message . "Account-Id : " . $stripe{"account-id"};
        }
    } else {
         $message = <<EOF;
Amount ....: $stripe{"Amount"} $stripe{"Currency"}
Recurring .: $recur_text
Desc ......: $stripe{"Desc"}
Cardno.....: *$stripe{"Last4"}
Processor .: Stripe
Charge-Id .: $stripe{"Charge-Id"}
Timestamp .: $stripe{"_timestamp"}
Email .....: $stripe{"Email"}
EOF
        if ($stripe{"account-id"} ne '') {
            $message = $message . "Account-Id : " . $stripe{"account-id"};
        }
    }

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

    # If the session has a lang value use that.
    if ($data{"lang"} ne '') {
        $lang = $data{"lang"};
    }

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

    # If the session has a lang value use that.
    if ($data{"lang"} ne '') {
        $lang = $data{"lang"};
    }

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
    $recur = $data{"Recur"};
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

    # If the session has a lang value use that.
    if ($data{"lang"} ne '') {
        $lang = $data{"lang"};
    }

    $request{"Currency"} = $data{"Currency"};
    $request{"Amount"} = $data{"Amount"};
    $request{"Desc"} = "GnuPG SEPA donation";
    $request{"Email"} = $data{"Mail"} unless $data{"Mail"} eq '';
    $request{"Meta[name]"} = $data{"Name"} unless $data{"Name"} eq 'Anonymous';
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
    $recur = $data{"Recur"};
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
elsif ($mode eq 'preset') {
    # Show a a template with certain preset values.
    $currency = 'EUR';
    $recur = '12';
    $paytype = 'cc';
    if ($q->param('plan') eq '12-5-eur' ) {
        $amount = '5';
    }
    elsif ($q->param('plan') eq '12-10-eur' ) {
        $amount = '10';
    }
    elsif ($q->param('plan') eq '12-20-eur' ) {
        $amount = '20';
    }

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
