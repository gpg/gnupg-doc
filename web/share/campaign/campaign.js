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

/* Defer loading Youtube iframe until the user clicks on the video.  */
$(document).ready(function() {
  /* For the video preview, we use this for devices without hover events.  */
  if ("ontouchstart" in document.documentElement) {
    $("body").addClass("touch");
  }

  /* Click handler for all videos.  */
  $(".camp-video").one("click", function() {
    let yt_id = $(this).data("embed");
    $(this).html('<iframe class="embed-responsive-item" allowfullscreen src="https://www.youtube.com/embed/' + yt_id + '?autoplay=1&rel=0"></iframe>');
  });
});

/* Fill donation amounts w/ javascript if possible.  */
$(document).ready(function() {
  let vals = ["500", "200", "100", "50", "20", "10", "5"];

  for (let value of vals) {
    $(".amount-btn-" + value.toString()).attr("href", "#");
    $(".amount-btn-" + value.toString()).on("click", function() {
      $("#amountother").attr("value", value.toString());
      $("#currency option").attr("selected", false);
      $("#currency option[value='EUR']").attr("selected", true);
      $("#recur option").attr("selected", false);
      $("#recur option[value='12']").attr("selected", true);
    });
  }
});
