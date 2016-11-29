// © 2010 EPFL/LAMP
// code by Gilles Dubochet, Felix Mulder

function Scheduler() {
    var scheduler = this;
    var resolution = 0;
    this.timeout = undefined;
    this.queues = new Array(0); // an array of work packages indexed by index in the labels table.
    this.labels = new Array(0); // an indexed array of labels indexed by priority. This should be short.

    this.label = function(name, priority) {
        this.name = name;
        this.priority = priority;
    }

    this.work = function(fn, self, args) {
        this.fn = fn;
        this.self = self;
        this.args = args;
    }

    this.addLabel = function(name, priority) {
        var idx = 0;
        while (idx < scheduler.queues.length && scheduler.labels[idx].priority <= priority) { idx = idx + 1; }
        scheduler.labels.splice(idx, 0, new scheduler.label(name, priority));
        scheduler.queues.splice(idx, 0, new Array(0));
    }

    this.clearLabel = function(name) {
        var idx = scheduler.indexOf(name);
        if (idx != -1) {
            scheduler.labels.splice(idx, 1);
            scheduler.queues.splice(idx, 1);
        }
    }

    this.nextWork = function() {
        var fn = undefined;
        var idx = 0;
        while (idx < scheduler.queues.length && scheduler.queues[idx].length == 0) { idx = idx + 1; }

        if (idx < scheduler.queues.length && scheduler.queues[idx].length > 0)
            var fn = scheduler.queues[idx].shift();

        return fn;
    }

    this.add = function(labelName, fn, self, args) {
        var doWork = function() {
            scheduler.timeout = setTimeout(function() {
                var work = scheduler.nextWork();
                if (work != undefined) {
                    if (work.args == undefined) { work.args = new Array(0); }
                    work.fn.apply(work.self, work.args);
                    doWork();
                }
                else {
                    scheduler.timeout = undefined;
                }
            }, resolution);
        }

        var idx = scheduler.indexOf(labelName)
        if (idx != -1) {
            scheduler.queues[idx].push(new scheduler.work(fn, self, args));
            if (scheduler.timeout == undefined) doWork();
        } else {
            throw("queue for add is non-existent");
        }
    }

    this.clear = function(labelName) {
        scheduler.queues[scheduler.indexOf(labelName)] = new Array();
    }

    this.indexOf = function(label) {
        var idx = 0;
        while (idx < scheduler.labels.length && scheduler.labels[idx].name != label)
            idx++;

        return idx < scheduler.queues.length && scheduler.labels[idx].name == label ? idx : -1;
    }

    this.queueEmpty = function(label) {
        var idx = scheduler.indexOf(label);
        if (idx != -1)
            return scheduler.queues[idx].length == 0;
        else
            throw("queue for label '" + label  + "' is non-existent");
    }

    this.scheduleLast = function(label, fn) {
        if (scheduler.queueEmpty(label)) {
            fn();
        } else {
            scheduler.add(label, function() {
                scheduler.scheduleLast(label, fn);
            });
        }
    }

    this.numberOfJobs = function(label) {
        var index = scheduler.indexOf(label);
        if (index == -1) throw("queue for label '" + label + "' non-existent");

        return scheduler.queues[index].length;
    }
};
