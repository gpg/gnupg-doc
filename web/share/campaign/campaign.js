/* Javascript for campaign related pages
 *
 * This code is Copyright 2017 The GnuPG Project and licensed
 * under a Creative Commons Attribution-ShareAlike 4.0 International
 * License.  See the file copying.org for details.
 */

/* For mobile devices in landscape orientation, the navbar is in the
   way of the video.  So hide it automatically then.  */
$(document).ready(function() {
  $(".navbar-fixed-top").autoHidingNavbar({
    disableAutohide: true,
    hideOffset: 20
  });
  check_autohide = function(event) {
    let hide = (screen.width < 768) && (screen.height < screen.width);
    $(".navbar-fixed-top").autoHidingNavbar("setDisableAutohide", !hide);
    if (!hide) {
      $(".navbar-fixed-top").autoHidingNavbar("show");
    }
  };
  $(window).on('orientationchange', check_autohide);
  check_autohide();
});

/* Random starting points for the testimonial carousel.  */
$(document).ready(function() {
  let nr_items_lg = 3 * $("#myCarousel div.item").length;
  let nr_items_md = 2 * $("#myCarouselMedium div.item").length;
  let nr_items_xs = 1 * $("#myCarouselSmall div.item").length;
  let nr_items = Math.min(nr_items_lg, nr_items_md, nr_items_xs);

  let active_item = Math.trunc(nr_items * Math.random());

  let active_slide_lg = Math.trunc(active_item / 3);
  let active_slide_md = Math.trunc(active_item / 2);
  let active_slide_xs = Math.trunc(active_item / 1);

  $("#myCarousel div.item").removeClass("active");
  $("#myCarouselMedium div.item").removeClass("active");
  $("#myCarouselSmall div.item").removeClass("active");

  $("#myCarousel div.item").eq(active_slide_lg).addClass("active");
  $("#myCarouselMedium div.item").eq(active_slide_md).addClass("active");
  $("#myCarouselSmall div.item").eq(active_slide_xs).addClass("active");
});

/* Advance carousel by swiping.  */
$(document).ready(function() {
  $('.carousel').bcSwipe({ threshold: 50 });
});


/* Fill donation amounts w/ javascript if possible.  */
$(document).ready(function() {
  let vals = ["500", "200", "100", "50", "20", "10", "5"];

  for (let idx = 0; idx < vals.length; ++idx) {
    let value = vals[idx];
    $(".amount-btn-" + value.toString()).attr("href", "#");
    $(".amount-btn-" + value.toString()).on("click", function(event) {
      $("#amountother").prop("value", value.toString());
      $("#currency option").prop("selected", false);
      $("#currency option[value='EUR']").prop("selected", true);
      //$("#recur option").prop("selected", false);
      //$("#recur option[value='12']").prop("selected", true);
      event.preventDefault();
    });
  }
});

function get_param_from_url(name) {
    let params = location.search.substring(1); // Snip away the ?
    params = params.split('&');
    let idx = 0;
    for (; idx < params.length; idx++) {
	let param = params[idx].split('=');
	if (param[0] != name) {
	    return;
	}
	if (param.length > 1) {
	    return decodeURIComponent(param[1]);
	}
	return "";
    }
}

/* Defer loading Youtube iframe until the user clicks on the video.  */
$(document).ready(function() {

    // VOTD: Update VOTD here.
    let VIDLIST = "sze,rysiek,ksenia,cindy,matt,thenmozhi,alex,andre,benjamin";

    let YTID = { "main": "wNHhkntqklg",
		 "thenmozhi": "sQMj332dgIE",
		 "sze": "OpeFuKRYGVA",
		 "sheera": "zwPaVA4vhDM",
		 "seanus": "H6iO_MkOICM",
		 "noah": "neibFsqgxgw",
		 "michael": "w4PY1ihLm0w",
		 "matt": "5MCGTd8pOG4",
		 "lisa": "Vd8sz5X-1og",
		 "john": "xdVHQhWrIro",
		 "jason": "RtvlfTiSEMc",
		 "geoffrey": "Y4yat43CvEc",
		 "daniel": "coFFCJlMRjk",
		 "cindy": "IdCiJMc3q80",
		 "benjamin": "atFz16nInIs",
		 "arthur": "Js_OqRLm9F4",
		 "andrew": "DXiU9wewjn4",
		 "andre": "bcNLlWqZ9d0",
		 "alex": "1OMJWpdl0DA",
                 "ksenia": "Qqg3_a72aEw",
                 "rysiek": "6DqfWz-KHSI"
	       };

    /* For the video preview, we use this for devices without hover events.  */
    if ("ontouchstart" in document.documentElement) {
	$("body").addClass("touch");
    }

    let wanted_yt_id = get_param_from_url('play');
    $(".camp-video").each(function() {
	let embed = $(this).data("embed");
	if (embed != 'votd') {
	    return;
	}
	let yt_ids = VIDLIST.split(",");
	let yt_id_idx = yt_ids.indexOf(wanted_yt_id);
	if (yt_id_idx == -1) {
	    wanted_yt_id = yt_ids[0];
	}
	$(this).data("embed", wanted_yt_id);
	$(this).children("img").attr("src", "/share/campaign/img/thumbs/" + wanted_yt_id + ".jpg");
    });

    /* To download the thumbs in share/campaign/img/thumbs:
       for f in YTID1 YTID2 ...; do wget -O $f.jpg http://i1.ytimg.com/vi/$f/maxresdefault.jpg; done # or hqdefault.jpg */

    /* Click handler for all videos.  */
    $(".camp-video").one("click", function() {
	let embed = $(this).data("embed");
	if (embed == 'votd') {
	    embed = VIDLIST;
	}
	let yt_id = embed.split(",")[0];
	yt_id = YTID[yt_id]; // What if key does not exist?
	let yt_list = $(this).data("embed-list");
	let extra_parms = "";
	if (yt_list) {
	    extra_parms = "&list=" + yt_list;
	}
	if (screen.width < 768) {
	    extra_parms = extra_parms + "&cc_load_policy=1"
	}
	$(this).html('<iframe class="embed-responsive-item" allowfullscreen src="https://www.youtube.com/embed/'
		     + yt_id + '?autoplay=1&modestbranding=1&rel=0' + extra_parms + '"></iframe>');
    });
});

/* hide the note about recurring donations under the Paypal option.  */
$(document).ready(function() {
  $("#recur").change(function() {
    console.log
    if (this.value === "0") {
      $("#paypal-note").hide();
    }
    else {
      $("#paypal-note").show();
    }
  });
});
