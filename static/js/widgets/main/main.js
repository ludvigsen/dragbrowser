define([
    'jquery',
    'underscore',
    'handlebars',
	'backbone',
    'templates'
	], function ($, _, Handlebars, Backbone, temp) {
    "use strict";
	var Main = {};
	Main.Model = Backbone.Model.extend({
    });

    Main.Collection = Backbone.Collection.extend({
        url: "http://oij.me:8000/api/%2F",
        model: Main.Model
    });

	Main.View = Backbone.View.extend({
        tagName: 'div',
        template : temp,
        //className: "main",
        collection: new Main.Collection(),
        //model: Main.Model,
        initialize: function () {
	        var that = this;
            this.collection.fetch({
                success: function () {
                    that.collection.each(function (data, index) {
                        //console.log(index);
                        //console.log(data.get("value"));
                        data.set("url", "http://oij.me:8000/download/" + encodeURIComponent(data.get("value")));
                    });
                    that.render();
                }
            });
        },
        events: {
            "click li": "clicked",
            "dragover": "dragover",
            "dragenter": "dragenter",
            "dragleave": "dragleave",
            "dragstart li": "dragstart",
            "drop": "drop"
        },
        dragstart: function (e) {
            //console.log("dragstart", e);
            //var fileDetails = 
            // Some forward thinking, utilise the custom data attribute to extend attributes available.
            /*var fileDetails = [];
            if(typeof files[0].dataset === "undefined") {
                // Grab it the old way
                fileDetails[0] = files[0].getAttribute("data-downloadurl");
                fileDetails[1] = files[1].getAttribute("data-downloadurl");
            } else {
                fileDetails[0] = files[0].dataset.downloadurl;
                fileDetails[1] = files[1].dataset.downloadurl;
            }*/
            console.log(e.currentTarget);
            console.log($(e.currentTarget).data("downloadurl"));
            e.originalEvent.dataTransfer.setData("DownloadURL", $(e.currentTarget).data("downloadurl"));
            //e.originalEvent.dataTransfer.setData("DownloadURL",fileDetails);
                    //evt.dataTransfer.setData("DownloadURL",fileDetails[0]);
            //                                                                                                                      },false);
            //                                                                                                                              files[1].addEventListener("dragstart",function(evt){
            //                                                                                                                                          evt.dataTransfer.setData("DownloadURL",fileDetails[1]);
            //                                                                                                                                                  },false);
            //
        },
        drop: function (e) {
            e.preventDefault();
            var files = e.originalEvent.target.files || e.originalEvent.dataTransfer.files;

            // process all File objects
            for (var i = 0, file; file = files[i]; i++) {
                console.log(file.name);
                console.log(file.type);
                console.log(file.size);

                var formData = new FormData();
                formData.append('file', file);
                formData.append('path', "/home/marius/" + file.name);

                var xhr = new XMLHttpRequest();
                xhr.open('POST', '/upload');
                xhr.onload = function() {
                    console.log(xhr.responseText);
                };
                xhr.upload.onprogress = function(event) {
                    console.log("onprogress");
                    if (event.lengthComputable) {
                        var complete = (event.loaded / event.total * 100 | 0);
                        console.log("onprogress", complete);
                        //updates a <progress> tag to show upload progress
                        //$('progress').val(complete);
                    }
                };
                xhr.send(formData);
                //  ...
            }
            console.log("drop");
        },
        dragover: function (e) {
            e.preventDefault();
            console.log("dragover");
        },
        dragenter: function (e) {
            //e.preventDefault();
            console.log("dragenter");
        },
        dragleave: function (e) {
            //e.preventDefault();
            console.log("dragleave");
        },
        clicked: function (e) {
            e.preventDefault();
            var name = $(e.currentTarget).data("id");
            if (name.slice(-2) === "..") {
                //Do something
                console.log(name);
                name = name.substr(0, name.lastIndexOf("/"));
                name = name.substr(0, name.lastIndexOf("/"));
                console.log(".. ", name);
            }
            if (name.slice(-1) === ".") {
                return;
            }
            console.log("changing url", name);
            var that = this;
            this.collection.fetch({
                url: "http://oij.me:8000/api/" + encodeURIComponent(name),
                success: function (data) {
                    that.collection.each(function (data, index) {
                        //console.log(index);
                        //console.log(data.get("value"));
                        data.set("url", "http://oij.me:8000/download/" + encodeURIComponent(data.get("value")));
                    });

                    console.log("rerender", data);
                    that.render();
                }
            });
        },
        render: function () {
            //console.log(this.collection.toJSON());
            this.$el.html(this.template({dirs: this.collection.toJSON()}));
            //this.$el.addClass(this.className);
        }
    });
    return Main;

});
