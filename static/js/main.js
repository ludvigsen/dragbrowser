require.config({
    baseUrl: "/js/",
    enforceDefine: true,
	paths: {
        jquery: 'jquery-2.0.3'
	},
	shim: {
        underscore: {
            deps: [],
            exports: '_'
        },
        handlebars: {
            deps: [],
            exports: 'Handlebars'
        },
		backbone: {
			deps: ['underscore', 'jquery'],
			exports: 'Backbone'
		}
	}
});

define(["jquery", "widgets/main/main"], function ($, Main) {
    console.log(Main);
    console.log("test!");
    var view = new Main.View();
    view.initialize();
    $("#main").append(view.el);
    //$("#main").replaceWith(Main.View.prototype.initialize());
	//console.log($);
});
