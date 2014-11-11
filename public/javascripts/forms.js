function submitForm() {
    $('.progress').show();
    $('#submit-button').attr("disabled", "disabled");
    $('#main-form').attr('action', $('#action').val());
    $('#main-form').submit();
}

function initAutoComplete() {
    $('#main-form').keydown(function() {
        if (event.keyCode == 13) {
            submitForm();
        }
    });
    $('.progress').hide();
    $('#submit-button').click(function () {
        submitForm();
    });

    var names = new Bloodhound({
        datumTokenizer: function (d) {
            return Bloodhound.tokenizers.whitespace(d.name);
        },
        queryTokenizer: Bloodhound.tokenizers.whitespace,
        local: nameList,
        limit:10
    });

    names.initialize();
    var nameTypeAhead = $('#name');

    nameTypeAhead.typeahead({
        highlight: true
    }, {
        name: 'name',
        displayKey: 'name',
        source: names.ttAdapter()
    });

    var selectedHandler = function (eventObject, suggestionObject, suggestionDataset) {
        $('#pid').val(suggestionObject.pid);
    };

    nameTypeAhead.on('typeahead:selected', selectedHandler);

}