$(document).ready(function(){
  /*
  This is adapted from Sam & Zach -
  http://buildinternet.com/2009/01/how-to-make-a-smooth-animated-menu-with-jquery/

  The use of stop() below is per -
  http://www.learningjquery.com/2009/01/quick-tip-prevent-animation-queue-buildup/

  @todo maybe make it a jquery poogin

  issues: if the mouse moves sufficiently fast, there is no mouseout event so
  the menu stays open until etc.
  */

  $("a").click(function(){   //Remove outline from links
    $(this).blur();
  });

  // var puts = (window.console && console.log) ? console.log : function(){};
  var puts = function(){};

  var menus = [];
  var registerMenu = function(menu){
    menu.data('identifier', menus.length);
    menus.push(menu);
  };

  var findHotspotFromSepDiv = function(sepDiv){
    var a = sepDiv.prev().find('a');
    if (a.length) return a;
    return sepDiv; // this is for items on level1 that have children and that
                   // we are at currently. (such items aren't hrefs, just text.)
  }

  var hideAnyShowing = function(){
    for (var i=menus.length; i--;) {
      var menu = menus[i];
      if (menu.data('showing')) {
        menu.data('hideMenu')(); // eew
      }
    }
  };

  var setupHotspotAndMenu = function(hotspot, menu){
    registerMenu(menu);
    var shortHeight, tallHeight, easing, duration;
    shortHeight = '0px';
    tallHeight = menu.height() + 'px';
    easing = 'easeOutBounce';
    duration = 600;
    menu.height(shortHeight);
    menu.hide();
    var hide = function(){ menu.hide(); };
    var showMenu = function(){
      hideAnyShowing();
      menu.data('showing', true);
      menu.show();
      menu.css({opacity: 1});
      menu.stop().animate(
        { height : tallHeight },
        { queue:false, duration:duration, easing:easing }
      );
    };
    var hideMenu = function(){
      menu.data('showing', false);
      menu.data('inMenu', false);
      menu.stop().animate(
        { height : shortHeight },
        { queue:false, duration:duration, easing:easing, complete:hide }
      );
      menu.animate(
        { opacity: 0 },
        { duration: duration }
      );
    };
    menu.data('hideMenu',hideMenu); // eew

    var hotspotMouseover = function(){
      if (menu.data('showing')){
        puts("hs mouseover - showing so nothing.");
      } else {
        puts("hs mouseover - not showing so show.");
        showMenu();
      }
    };
    var hotspotMouseout = function(){
      // hack - we need to wait a few beats to see if the mouse goes
      // into the menu or not.
      setTimeout(function(){
        if (menu.data('inMenu')) {
          puts("hotspot mouseout - in menu so stay.");
        } else {
          puts("hotspot mouseout - not in menu so hide.");
          hideMenu();
        }
      },600);
    };
    var menuMouseover = function(e, e2){
      if (!e) { e = e2; }
      // this hits a lot b/c of all the elements in menu.  we take them all
      menu.data('inMenu', true);
    };
    var menuMouseout = function(e, e2){
      if (!e) { e = e2; }
      if (e.target != menu[0]){
        // puts("menu mouseout - ignoring b/c menu was not target."); too noisy
      } else {
        puts("menu mouseout - always hides menu when menu was target.");
        hideMenu();
      }
    };
    hotspot.mouseover(hotspotMouseover);
    hotspot.mouseout(hotspotMouseout);
    menu.mouseover(menuMouseover);
    menu.mouseout(menuMouseout);
  };

  els = $('.bouncy-lvl2-menu');
  for (var i=els.length; i--;) {
    var menu = $(els[i]);
    var sepDiv = menu.parent();
    var hotspot = findHotspotFromSepDiv(sepDiv);
    setupHotspotAndMenu(hotspot, menu);
  }

});
