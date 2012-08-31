/*
Contains code for adding Freebase Suggest to the 
OpenIE demo. 
*/

var arg1Suggest = false;
var arg2Suggest = false;

var alert = false;

$(document).ready(function() {
  $("#arg1").on('keyup', attachSuggest);
  $("#arg2").on('keyup', attachSuggest);

  $("#arg1").on('keyup', argHelper);
  $("#rel").on('keyup', relHelper);
  $("#arg2").on('keyup', argHelper);
});

function argHelper() {

  var box = $(this)
  var text = box.val().toLowerCase();
  // to keep track of what's in the other box
  var otherText = "";
  if (box.attr("id") == "arg1") {
    otherText = $("#arg2").val().toLowerCase();
  } else {
    otherText = $("#arg1").val().toLowerCase();
  }

  if (alert) {
    // see if the alert is still necessary
    if (text.indexOf("which ") != 0 && text.indexOf("who") != 0 && text.indexOf("what") != 0 && text.indexOf("where") != 0 &&
        otherText.indexOf("which ") != 0 && otherText.indexOf("who") != 0 && otherText.indexOf("what") != 0 && otherText.indexOf("where") != 0) {
      $(".alert").alert('close');
    }

  } else {
    // see if an alert is necessary 
    if (text.indexOf("which ") == 0) {
      var alertDiv = getAlert("<strong>Warning: </strong>Queries starting with \"which\" rarely return results. Try a type query instead, with \"type:\"");
      $("#query-well").append(alertDiv);
      alert = true;
    } else if (text.indexOf("who") == 0) {
      var alertDiv = getAlert("<strong>Warning: </strong>Instead of searching for \"who\", try \"type:person\" or leave the box blank.");
      $("#query-well").append(alertDiv);
      alert = true;
    } else if (text.indexOf("what") == 0) {
      var alertDiv = getAlert("<strong>Warning: </strong>Searching for \"what\" is unnecessary. Try reforming your query if necessary.");
      $("#query-well").append(alertDiv);
      alert = true;
    } else if (text.indexOf("where") == 0 && box.attr("id") == "arg1") {
      var alertDiv = getAlert("<strong>Warning: </strong>If searching for \"where, is, x\", reform your query to the form \"x, is located in, (blank)\" for better results.");
      $("#query-well").append(alertDiv);
      alert = true;
    }

  }
}

function relHelper() {

  if (alert) {
    return;
  }

  var box = $(this)
  var text = box.val().toLowerCase();

}

function getAlert(str) {
  var div = $(document.createElement("div"));
  div.addClass("alert fade in")
  
  var button = $(document.createElement("button"));
  button.addClass("close")
  .attr("type", "button")
  .attr("data-dismiss", "alert")
  .text("×");

  div.html(str);
  div.append(button);

  div.bind('closed', function() { alert = false; });
  return div;
}

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
