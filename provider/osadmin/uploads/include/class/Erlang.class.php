<?php
if(!defined('ACCESS')) {exit('Access denied.');}

##    ~a - an atom
##    ~s - a string
##    ~b - a binary (contain 0x0 in string)
##    ~i - an integer
##    ~l - a long integer
##    ~u - an unsigned long integer
##    ~f - a float
##    ~d - a double float
##    ~p - an erlang pid


class Erlang extends Base{

    $link = peb_connect('sadly-desktop@sadly-desktop',  'secret');
    if (!$link) {
        die('Could not connect: ' . peb_error());
    }

    $msg = peb_encode('[~p,~a]', array(
        array($link,'getinfo')
    )
);
    //The sender must include a reply address.  use ~p to format a link identifier to a valid Erlang pid.

    peb_send_byname('pong',$msg,$link);

    $message = peb_receive($link);
    $rs= peb_decode( $message) ;
    print_r($rs);

    $serverpid = $rs[0][0];

    $message = peb_encode('[~s]', array(
        array( 'how are you')
    )
);
    peb_send_bypid($serverpid,$message,$link);
    //just demo for how to use peb_send_bypid

    peb_close($link);
}
?>
