module.exports = function(grunt){
    grunt.initConfig({
        handlebars: {
            compile: {
                options: {
                    namespace: false,
                    amd: true
                },
                files: {
                    "templates.js": ["widgets/**/*.hbs"]
                }
            }
		}
    });
    grunt.loadNpmTasks('grunt-contrib-handlebars');
    grunt.registerTask('build', ['handlebars']);
}
