/*
   Copyright (c) 2005-2010, York Liu <sadly@phpx.com>, Alvaro Videla <videlalvaro@gmail.com>
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
met:

 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.
 * The names of the contributors may not be used to endorse or promote products
 derived from this software without specific prior written
 permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* $Id: header,v 1.16.2.1.2.1 2007/01/01 19:32:09 iliaa Exp $ */

#ifndef PHP_ROPEB_H
#define PHP_ROPEB_H

#define PHP_ROPEB_EXTNAME "ropeb"
#define PHP_ROPEB_VERSION "0.1"


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"

//Erlang Libraries
#include "ei.h"
#include "erl_interface.h"

/****************************************
  Errors
 ****************************************/
#define ROPEB_ERL_
#define ROPEB_ERRORNO_INIT 1
#define ROPEB_ERROR_INIT		"ei_connect_init error"
#define ROPEB_ERRORNO_CONN 2
#define ROPEB_ERROR_CONN		"ei_connect error"
#define ROPEB_ERRORNO_SEND 3
#define ROPEB_ERROR_SEND		"ei_send error"
#define ROPEB_ERRORNO_RECV 4
#define ROPEB_ERROR_RECV		"ei_receive error"
#define ROPEB_ERRORNO_NOTMINE 5
#define ROPEB_ERROR_NOTMINE		 "ei_receive got a message but not mine"
#define ROPEB_ERRORNO_DECODE 6
#define ROPEB_ERROR_DECODE		"ei_decode error, unsupported data type"

/****************************************
  Resource names
 ****************************************/
#define ROPEB_RESOURCENAME		"PHP-Erlang Bridge"
#define ROPEB_TERMRESOURCE		"Erlang Term"
#define ROPEB_SERVERPID				 "Erlang Pid"

#define ROPEB_DEFAULT_TMO			 1000
// #define DEBUG_PRINTF 1

extern zend_module_entry ropeb_module_entry;
#define phpext_ropeb_ptr &ropeb_module_entry

#ifdef PHP_WIN32
#define PHP_ROPEB_API __declspec(dllexport)
#else
#define PHP_ROPEB_API
#endif

#ifdef ZTS
#include "TSRM.h"
#endif

PHP_MINIT_FUNCTION(ropeb);
PHP_MSHUTDOWN_FUNCTION(ropeb);
PHP_RINIT_FUNCTION(ropeb);
PHP_RSHUTDOWN_FUNCTION(ropeb);
PHP_MINFO_FUNCTION(ropeb);

PHP_FUNCTION(ropeb_connect);
PHP_FUNCTION(ropeb_pconnect);
PHP_FUNCTION(ropeb_close);
PHP_FUNCTION(ropeb_send_byname);
PHP_FUNCTION(ropeb_rpc);
PHP_FUNCTION(ropeb_rpc_to);
PHP_FUNCTION(ropeb_send_bypid);
PHP_FUNCTION(ropeb_receive);
PHP_FUNCTION(ropeb_encode);
PHP_FUNCTION(ropeb_vencode);
PHP_FUNCTION(ropeb_decode);
PHP_FUNCTION(ropeb_vdecode);
PHP_FUNCTION(ropeb_error);
PHP_FUNCTION(ropeb_errorno);

PHP_FUNCTION(ropeb_linkinfo);

PHP_FUNCTION(ropeb_status);

PHP_FUNCTION(ropeb_print_term);
PHP_FUNCTION(ropeb_system);

ZEND_BEGIN_MODULE_GLOBALS(ropeb)
		// char *default_nodename;
		// char *default_cookie;
		// long default_timeout;

		long default_link;
		long num_link,num_persistent;
		long max_link,max_persistent;

		long errorno;
		char *error;

		long instanceid;
ZEND_END_MODULE_GLOBALS(ropeb)

		/* In every utility function you add that needs to use variables
		   in php_ropeb_globals, call TSRMLS_FETCH(); after declaring other
		   variables used by that function, or better yet, pass in TSRMLS_CC
		   after the last function argument and declare your utility function
		   with TSRMLS_DC after the last declared argument.  Always refer to
		   the globals in your function as ROPEB_G(variable).  You are
		   encouraged to rename these macros something shorter, see
		   examples in any other php module directory.
		   */

#ifdef ZTS
#define ROPEB_G(v) TSRMG(ropeb_globals_id, zend_ropeb_globals *, v)
#else
#define ROPEB_G(v) (ropeb_globals.v)
#endif

#endif	/* PHP_ROPEB_H */


		/*
		 * Local variables:
		 * tab-width: 4
		 * c-basic-offset: 4
		 * End:
		 * vim600: noet sw=4 ts=4 fdm=marker
		 * vim<600: noet sw=4 ts=4
		 */
