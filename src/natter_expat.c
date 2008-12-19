// Copyright 2008, Engine Yard, Inc.
//
// This file is part of Natter.
//
// Natter is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// Natter is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with Natter.  If not, see <http://www.gnu.org/licenses/>.


#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>
#include <expat.h>

#define XML_START "element_start"
#define XML_END   "element_end"
#define XML_CDATA "cdata"
#define XML_ERROR "error"

ei_x_buff event_buf;

typedef struct {
  ErlDrvPort port;
  XML_Parser parser;
} natter_drv;

void *natterExpat_EmitStartElement(const XML_Char *name) {
  ei_x_encode_list_header(&event_buf, 1);
  ei_x_encode_tuple_header(&event_buf, 2);
  ei_x_encode_atom(&event_buf, XML_START);
  ei_x_encode_tuple_header(&event_buf, 2);
  ei_x_encode_string(&event_buf, name);
  return NULL;
}

void *natterExpat_EmitAttributes(const XML_Char **attrs) {
  int i = 0, j = 0;
  int attr_count = 0;
  // Count the name/value pairs
  for (i = 0; attrs[i]; i += 2) {}
  if (i > 0) {
    // And then divide by two to get the
    // attribute count
      attr_count = i / 2;
    // Encode attributes as a proplist
    ei_x_encode_list_header(&event_buf, attr_count);
    for (j = 0; attrs[j]; j += 2) {
      ei_x_encode_tuple_header(&event_buf, 2);
      ei_x_encode_string(&event_buf, attrs[j]);
      ei_x_encode_string(&event_buf, attrs[j + 1]);
    }
  }
  ei_x_encode_empty_list(&event_buf);
  return NULL;
}

void *natterExpat_EmitEndElement(const XML_Char *name) {
  ei_x_encode_list_header(&event_buf, 1);
  ei_x_encode_tuple_header(&event_buf, 2);
  ei_x_encode_atom(&event_buf, XML_END);
  ei_x_encode_string(&event_buf, name);
  return NULL;
}

void *natterExpat_CDATA(const XML_Char *s, int len) {
  ei_x_encode_list_header(&event_buf, 1);
  ei_x_encode_tuple_header(&event_buf, 2);
  ei_x_encode_atom(&event_buf, XML_CDATA);
  ei_x_encode_binary(&event_buf, s, len);
  return NULL;
}

void *natterExpat_StartElementHandler(natter_drv *d,
				      const XML_Char *name,
				      const XML_Char **attrs) {
  natterExpat_EmitStartElement(name);
  natterExpat_EmitAttributes(attrs);
  return NULL;
}

void *natterExpat_EndElementHandler(natter_drv *d,
				    const XML_Char *name) {
  natterExpat_EmitEndElement(name);
  return NULL;
}

void *natterExpat_CharacterDataHandler(natter_drv *d,
				       const XML_Char *s,
				       int len) {
  natterExpat_CDATA(s, len);
  return NULL;
}


static ErlDrvData natter_expat_start(ErlDrvPort port, char *buff) {
  natter_drv* d = (natter_drv*)driver_alloc(sizeof(natter_drv));
  d->port = port;
  d->parser = XML_ParserCreate("UTF-8");
  XML_SetUserData(d->parser, d);

  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  XML_SetStartElementHandler(d->parser, (XML_StartElementHandler)natterExpat_StartElementHandler);
  XML_SetEndElementHandler(d->parser, (XML_EndElementHandler)natterExpat_EndElementHandler);
  XML_SetCharacterDataHandler(d->parser, (XML_CharacterDataHandler)natterExpat_CharacterDataHandler);

  return (ErlDrvData)d;
}

static void natter_expat_stop(ErlDrvData handle) {
  XML_ParserFree(((natter_drv *)handle)->parser);
  driver_free((char*)handle);
}

// Driver only knows how to parse XML
// All commands are ignored
static int natter_expat_control(ErlDrvData drv_data,
			     unsigned int command,
			     char *buf, int len,
			     char **rbuf, int rlen) {

  natter_drv* d = (natter_drv*)drv_data;
  int res, errcode;
  char *errstring;
  ErlDrvBinary *b;
  size_t size;

  ei_x_new_with_version(&event_buf);
  res = XML_Parse(d->parser, buf, len, 0);

  if(!res) {
    errcode = XML_GetErrorCode(d->parser);
    errstring = (char *)XML_ErrorString(errcode);

    ei_x_encode_list_header(&event_buf, 1);
    ei_x_encode_tuple_header(&event_buf, 2);
    ei_x_encode_atom(&event_buf, XML_ERROR);
    ei_x_encode_tuple_header(&event_buf, 2);
    ei_x_encode_long(&event_buf, errcode);
    ei_x_encode_string(&event_buf, errstring);
  }

  ei_x_encode_empty_list(&event_buf);
  size = event_buf.index;
  b = driver_alloc_binary(size);
  memcpy(b->orig_bytes, event_buf.buff, size);
  ei_x_free(&event_buf);
  *rbuf = (char *)b;
  return size;

}

ErlDrvEntry expat_driver_entry = {
  NULL,			/* F_PTR init, N/A */
  natter_expat_start,	/* L_PTR start, called when port is opened */
  natter_expat_stop,	/* F_PTR stop, called when port is closed */
  NULL,			/* F_PTR output, called when erlang has sent */
  NULL,			/* F_PTR ready_input, called when input descriptor ready */
  NULL,			/* F_PTR ready_output, called when output descriptor ready */
  "natter_expat",	/* char *driver_name, the argument to open_port */
  NULL,			/* F_PTR finish, called when unloaded */
  NULL,			/* handle */
  natter_expat_control,	/* F_PTR control, port_command callback */
  NULL,			/* F_PTR timeout, reserved */
  NULL			/* F_PTR outputv, reserved */
};

// must match name in driver_entry
DRIVER_INIT(natter_expat) {
  return &expat_driver_entry;
}
