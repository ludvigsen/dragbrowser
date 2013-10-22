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

    Main.ContextMenuView = Backbone.View.extend({
        tagName: 'div',
        className: "list-group",
        template: temp['widgets/main/main.contextmenu.hbs'],
        model: new Backbone.Model(),
        initialize: function () {
            this.menuItems = ["Rename", "Copy", "Cut", "Paste", "Delete", "Cancel"];
        },
        events: {
            "click .Rename" : "rename",
            "click .Copy"   : "copy",
            "click .Cut"    : "cut",
            "click .Paste"  : "paste",
            "click .Delete" : "del",
            "click .Cancel" : "cancel"
        },
        rename: function (e) {
            this.$el.remove();
            var li = this.model.get("currentTarget"),
                name = li.data("id").substr(li.data("id").lastIndexOf("/") + 1, li.data("id").length),
                inDir = li.data("id").substr(0, li.data("id").lastIndexOf("/")),
                input = '<input type="text" value="' + name +  '"></input>';
            input = $(input);
            li.empty();
            li.append(input);
            input.on("keyup", function (e) {
                var code = e.keyCode || e.which;
                    //encodeURIComponent(inDir + "/" + input.val()));
                if (code === 13) { //Enter keycode
                    $.get("/move/" + encodeURIComponent(inDir + "/" + name) + "/" +
                        encodeURIComponent(inDir + "/" + input.val()), function () {
                            //that.rerender();
                            Backbone.trigger("main:rerender");
                        });
                }
            });
        },
        copy: function () {
            this.$el.remove();
            if(!this.model.get("currentTarget"))
                return;
            var file = this.model.get("currentTarget").data("id");
            this.model.set("cutDirectory", undefined);
            this.model.set("copyDirectory",file);
        },
        cut: function () {
            this.$el.remove();
            if(!this.model.get("currentTarget"))
                return;
            var file = this.model.get("currentTarget").data("id");
            this.model.set("copyDirectory", undefined);
            this.model.set("cutDirectory",file);
        },
        paste: function () {
            this.$el.remove();
            var cutDir = this.model.get("cutDirectory"), copyDir = this.model.get("copyDirectory");
            var from, copy, to = this.model.get("currentDirectory");
            if (cutDir) {
                from = cutDir;
                copy = false;
                this.model.set("cutDirectory", undefined);
            }
            if (copyDir) {
                from = copyDir;
                copy = true;
                this.model.set("copy", undefined);
            }

            if (copy !== undefined && copy) {
                var toFile = to + from.substr(from.lastIndexOf("/"), from.length);
                $.get("/copy/" + encodeURIComponent(from) + "/" + encodeURIComponent(toFile), function () {
                    Backbone.trigger("main:rerender");
                });
            }
            if (copy !== undefined && !copy) {
                var toFile = to + from.substr(from.lastIndexOf("/"), from.length);
                $.get("/move/" + encodeURIComponent(from) + "/" + encodeURIComponent(toFile), function () {
                    Backbone.trigger("main:rerender");
                });
            }
        },
        del: function(){
            this.$el.remove();
            if(this.model.get("currentTarget")){
                var file = this.model.get("currentTarget").data("id");
                $.get("/delete/" + encodeURIComponent(file), function (){
                    Backbone.trigger("main:rerender");
                });
            }
        },
        cancel: function () {
            this.$el.remove();
        },
        render: function () {
            this.$el.html(this.template({menuItems: this.menuItems}));
        }

    });

    Main.View = Backbone.View.extend({
        tagName: 'div',
        template : temp["widgets/main/main.hbs"],
        collection: new Main.Collection(),
        currentDirectory: "/",
        initialize: function () {
            var that = this;
            Backbone.on("main:rerender", function(){
                that.rerender();
            });
            this.collection.fetch({
                success: function () {
                    that.collection.each(function (data, index) {
                        data.set("url", "http://oij.me:8000/download/" + encodeURIComponent(data.get("value")));
                        if (data.get("type") === "directory") {
                            data.set("directory", true);
                        }
                    });
                    that.render();
                }
            });
        },
        events: {
            "click li": "clicked",
            "contextmenu li": "rightclick",
            "dragover": "dragover",
            "dragenter": "dragenter",
            "dragleave": "dragleave",
            "dragstart li": "dragstart",
            "drop li": "drop"
        },
        rightclick: function (e) {
            e.preventDefault();
            var contextMenu = new Main.ContextMenuView();
            contextMenu.model.set("currentTarget", $(e.currentTarget));
            contextMenu.model.set("currentDirectory", this.currentDirectory);
            contextMenu.render();
            $("body").append(contextMenu.$el.css("width","200px"));
            console.log(contextMenu.$el);
            contextMenu.$el.offset({top: e.pageY, left: e.pageX});
        },
        dragstart: function (e) {
            this.currentDraggingDir = $(e.currentTarget);
            e.originalEvent.dataTransfer.setData("DownloadURL", $(e.currentTarget).data("downloadurl"));
        },
        drop: function (e) {
            console.log(e);
            e.preventDefault();
            var copyFrom = this.currentDraggingDir.data("id"),
                that = this,
                copyTo = $(e.currentTarget).data("id");

            var files = e.originalEvent.target.files || e.originalEvent.dataTransfer.files;

            if (files.length === 0 && copyFrom !== copyTo) {
                $.get("/move/" + encodeURIComponent(copyFrom) + "/" + encodeURIComponent(copyTo));
            }

            // process all File objects
            for (var i = 0, file; file = files[i]; i++) {
                var formData = new FormData();
                formData.append('file', file);
                formData.append('path', this.currentDirectory + "/"  + file.name);

                var xhr = new XMLHttpRequest();
                xhr.open('POST', '/upload');
                xhr.onload = function() {
                    console.log(xhr.responseText);
                };
                xhr.upload.onprogress = function(event) {
                    $("#hiddenFooter").show()
                        if (event.lengthComputable) {
                            var complete = (event.loaded / event.total * 100 | 0);
                            $("#progressBar").attr("aria-valuenow", complete);
                            $("#progressBar").width(complete+"%");
                            if(complete == 100) {
                                $("#hiddenFooter").hide();
                                that.rerender();
                            }
                            console.log("onprogress", complete);
                        }
                };
                xhr.send(formData);
            }
        },
        dragover: function (e) {
            e.preventDefault();
        },
        dragenter: function (e) {
        },
        dragleave: function (e) {
        },
        clicked: function (e) {
            if($(e.currentTarget).find("input").length > 0)
                return;
            e.preventDefault();
            var name = $(e.currentTarget).data("id");
            if (name.slice(-2) === "..") {
                //Do something
                name = name.substr(0, name.lastIndexOf("/"));
                name = name.substr(0, name.lastIndexOf("/"));
            }
            if (name.slice(-1) === ".") {
                return;
            }
            this.currentDirectory = name;
            var that = this;
            this.rerender();
        },
        rerender: function () {
            var that = this;
            this.collection.fetch({
                url: "http://oij.me:8000/api/" + encodeURIComponent(that.currentDirectory),
                success: function (data) {
                    that.collection.each(function (data, index) {
                        data.set("url", "http://oij.me:8000/download/" + encodeURIComponent(data.get("value")));
                        if (data.get("type") === "directory") {
                            data.set("directory", true);
                        }
                    });
                    that.render();
                }
            });

        },
        render: function () {
            this.$el.html(this.template({dirs: this.collection.toJSON()}));
        }
    });
    return Main;
});
