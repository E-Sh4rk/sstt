// import { send } from "sstt";

const version_file = "version.txt";

function getFile(url, success_callback) {
    let xhr = new XMLHttpRequest();
    xhr.open("GET", url);
    xhr.setRequestHeader("Cache-Control", "no-cache, no-store, max-age=0");
    xhr.overrideMimeType("text/plain");
    xhr.addEventListener("readystatechange", () => {
        if (xhr.readyState == 4) {
            if (xhr.status == 200)  {
                success_callback(xhr.responseText);
            } else {
                console.log("Cannot load file " + url);
            }
        }
    });
    xhr.send();
};

jQuery(function($, undefined) {
    let term = $('#terminal').terminal(function(command) {
        if (command !== '') {
            var result = sstt.send(command);
            if (result != null) {
                this.echo(result);
            }
        }
    }, {
        greetings: 'Simple Set-Theoretic Types (SSTT) - REPL',
        name: 'sstt',
        // height: 400,
        // width: 800,
        prompt: '> '
    });
    term.focus(true);
    function show_version(content) {
        if (typeof sstt === 'undefined') setTimeout(() => show_version(content), 100);
        else term.echo("Version " + sstt.version() + " - Commit " + sstt.commit() + " (cached) / " + content.trim() + " (latest) - Compiler " + sstt.compiler());
    }
    getFile(version_file, show_version);
});