/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_pantheios_be_colour_h_
#define _uhal_pantheios_be_colour_h_

#include "pantheios/pantheios.hpp"

PANTHEIOS_CALL ( int ) pantheios_be_init (
	char const* processIdentity
	, void*       reserved
	, void**      ptoken
);

PANTHEIOS_CALL ( void ) pantheios_be_uninit (
	void* token
);

PANTHEIOS_CALL ( int ) pantheios_be_logEntry (
	void*       feToken
	, void*       beToken
	, int         severity
	, char const* entry
	, size_t      cchEntry
);

#endif
