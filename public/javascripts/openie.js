/*
Contains code for adding Freebase Suggest to the 
OpenIE demo. 
 */

var arg1Suggest = false;
var arg2Suggest = false;

$(document).ready(function() {
  $("#arg1").on('keyup', attachSuggest);
  $("#arg2").on('keyup', attachSuggest);
});

/**
 * This function attaches suggest to an argument
 * box. Called via event handling. 
 */
function attachSuggest() {
  var box = $(this)
  var isArg1 = this.id == "arg1";
  var isArg2 = this.id == "arg2";
  var text = box.val().toLowerCase();

  if (text == "type:" || text.indexOf("type:") == 0) {
    if ((isArg1 && !arg1Suggest) || 
        (isArg2 && !arg2Suggest)) {
      // attach suggest and appropriate handlers for type queries
      box.suggest({
        key:'AIzaSyDERIKha5FgaoJPlOIRQMeBz8F6Qbwmtxg',
        filter:'(all type:/type/type with:commons)'
      }).on("fb-select", function(e, data) {
        box.val("type:" + data.name);
      });

      // attach appropriate query handling
      box.data("suggest").request = queryFunction;

      // modify boolean vars accordingly
      if (isArg1) arg1Suggest = true;
      if (isArg2) arg2Suggest = true;
    }
  } else if (text == "entity:" || text.indexOf("entity:") == 0) {
    if ((isArg1 && !arg1Suggest) || 
        (isArg2 && !arg2Suggest)) {
      // attach suggest and apprioriate handlers for entity queries
      box.suggest({
        key:'AIzaSyDERIKha5FgaoJPlOIRQMeBz8F6Qbwmtxg',
        filter:'(all (not type:/type/type))'
      }).on("fb-select", function(e, data) {
        box.val("entity:" + data.name);
      });

      // attach appropriate query handling
      box.data("suggest").request = queryFunction;

      // modify boolean vars accordingly
      if (isArg1) arg1Suggest = true;
      if (isArg2) arg2Suggest = true;
    }
  } else {
    // if suggest is attached, de-attach it.
    if (box.data("suggest")) {
      box.data("suggest")._destroy();
      if (isArg1) arg1Suggest = false;
      if (isArg2) arg2Suggest = false;
    }
  }
}

// function defined in suggest.js from freebase:
// copied the method to have manual control of 
// what queries were sent to freebase. instead
// of searching for "type:food" we can intercept
// the query, chop off "type:", and just query
// freebase for "food".
var queryFunction = function(val, cursor) {
  var self = this,
      o = this.options;

  var query = val;

  // modifications to this function so that type and entity queries are done properly.
  if (query.indexOf("type:") == 0) {
    query = query.substring(5, query.length);
  } else if (query.indexOf("entity:") == 0) {
    query = query.substring(7, query.length);
  }

  var filter = o.ac_param.filter || [];

  // SEARCH_PARAMS can be overridden inline
  var extend_ac_param = null;

  if ($.type(filter) === "string") {
      // the original filter may be a single filter param (string)
      filter = [filter];
  }
  // clone original filters so that we don't modify it
  filter = filter.slice();
  if (o.advanced) {
      // parse out additional filters in input value
      var structured = $.suggest.parse_input(query);
      query = structured[0];
      if (structured[1].length) {
          // all advance filters are ANDs
          filter.push("(all " + structured[1].join(" ") + ")");
      }
      extend_ac_param = structured[2];
      if ($.suggest.check_mql_id(query)) {
          // handle anything that looks like a valid mql id
          filter.push("(all mid:\"" + query + "\")");
          query = "";
      }
  }

  var data = {};
  data[o.query_param_name] = query;

  if (cursor) {
    data.cursor = cursor;
  }
  $.extend(data, o.ac_param, extend_ac_param);
  if (filter.length) {
      data.filter = filter;
  }

  var url = o.service_url + o.service_path + "?" + $.param(data, true);
  var cached = $.suggest.cache[url];
  if (cached) {
    this.response(cached, cursor ? cursor : -1, true);
    return;
  }

  clearTimeout(this.request.timeout);

  var ajax_options = {
    url: o.service_url + o.service_path,
    data: data,
    traditional: true,
    beforeSend: function(xhr) {
      var calls = self.input.data("request.count.suggest") || 0;
      if (!calls) {
        self.trackEvent(self.name, "start_session");
      }
      calls += 1;
      self.trackEvent(self.name, "request", "count", calls);
      self.input.data("request.count.suggest", calls);
    },
    success: function(data) {
      $.suggest.cache[url] = data;
      data.prefix = val;  // keep track of prefix to match up response with input value
      self.response(data, cursor ? cursor : -1);
    },
    error: function(xhr) {
      self.status_error();
      self.trackEvent(self.name, "request", "error", {
        url: this.url,
        response: xhr ? xhr.responseText : ''
      });
      self.input.trigger("fb-error", Array.prototype.slice.call(arguments));
    },
    complete: function(xhr) {
      if (xhr) {
        self.trackEvent(self.name, "request", "tid",
        xhr.getResponseHeader("X-Metaweb-TID"));
      }
    },
    dataType: "jsonp",
    cache: true
  };

  this.request.timeout = setTimeout(function() {
    $.ajax(ajax_options);
  }, o.xhr_delay);
}
