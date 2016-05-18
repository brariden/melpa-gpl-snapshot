/*
 * zeitgeistextend.h
 * This file is part of zeitgeist dataprovider for firefox 
 *
 * Copyright (C) 2010 - Markus Korn <thekorn@gmx.de>
 *
 * zeitgeist dataprovider for firefox is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * zeitgeist dataprovider for firefox is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
 
 
#ifndef _ZEITGEISTEXTEND_H_
#define _ZEITGEISTEXTEND_H_

#include "zeitgeist.h"
#include "zeitgeist-library.h"

#define ZEITGEIST_COMPONENT_CONTRACTID "@zeitgeist-project.com/DATAPROVIDER/firefox-xpcom;1"
#define ZEITGEIST_COMPONENT_CLASSNAME "zeitgeist dataprovider for firefox"
#define ZEITGEIST_COMPONENT_CID  { 0xd879c08c, 0x517d, 0x44f0, { 0x83, 0xe1, 0x3e, 0xf7, 0x5a, 0x52, 0x7d, 0xdf } }

//d879c08c-517d-44f0-83e1-3ef75a527ddf
class zeitgeistextend : public zeitgeist
{
	public:
		NS_DECL_ISUPPORTS
		NS_DECL_ZEITGEIST

		zeitgeistextend();
		virtual ~zeitgeistextend();
		ZeitgeistLog	*log;
	
};
#endif
