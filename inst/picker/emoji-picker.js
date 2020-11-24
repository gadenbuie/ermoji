import { EmojiButton } from './emoji-button.js' // @joeattardi/emoji-button

const pickerTypePlugin = {
  render(picker) {
    const choices = [
      {value: 'unicode', label: '<span>Unicode</span>'},
      {value: 'html', label: '<span>HTML</span>'},
      {value: 'emo_ji', label: '<span style="font-family: monospace;">emo::ji</span>'}
    ]

    function htmlToElems(html) {
      let temp = document.createElement('template');
      temp.innerHTML = html;
      return temp.content.childNodes;
    }

    function makeRadioElement({value, label}) {
      const div = document.createElement('div')
      const divLabel = document.createElement('label')
      divLabel.classList = 'radio-inline'

      const input = document.createElement('input')
      input.type = 'radio'
      input.name = 'picker_type'
      input.value = value

      if (document.getElementById('picker-gadget').matches('.' + value)) {
        input.checked = 'checked'
      }

      divLabel.appendChild(input)
      divLabel.appendChild(htmlToElems(label)[0])
      div.appendChild(divLabel)
      return div
    }

    const div = document.createElement('div')
    div.id = 'picker_type'

    const label = document.createElement('label')
    label.classList = 'control-label'
    label['for'] = 'picker_type'
    label.innerText = 'Insert as...'
    div.appendChild(label)

    choices.map(makeRadioElement).forEach(el => div.appendChild(el))

    return div;
  }
}

const picker = new EmojiButton({
  position: {
    top: '0',
    right: '0',
    bottom: '0',
    left: '0'
  },
  autoHide: false,
  theme: document.getElementById('picker-gadget').matches('.dark-theme') ? 'dark' : 'light',
  // plugins: [closePlugin, insertEmoji]
  plugins: [pickerTypePlugin]
})

picker.on('emoji', function(selection) {
  selection.html = he.encode(selection.emoji)
  Shiny.setInputValue('emoji', selection, { priority: 'event' });
});

picker.togglePicker(document.getElementById('emoji-picker'))

const divPicker = document.querySelector('.emoji-picker__wrapper')
divPicker.style.top = '0'
divPicker.style.left = '0'

divPicker.addEventListener('keydown', function(ev) {
  if (ev.keyCode === 27) {
    ev.stopPropagation()
    Shiny.setInputValue('close', Date.now())
  }
})


// Communicate choices of emoji type to insert ...
const pickerInput = document.getElementById('picker_type')

function reportPickerType() {
  const choices = [...pickerInput.querySelectorAll('input')]
  const value = choices.filter(item => item.checked).map(item => item.value)
  Shiny.setInputValue('picker_type', value[0])
}

function updatePickerType(value) {
  const choices = [...pickerInput.querySelectorAll('input')]
  choices.forEach(function(choice) {
    if (choice.value === value) {
      choice.checked = 'checked'
      Shiny.setInputValue('picker_type', value)
    } else {
      choice.removeAttribute('checked')
    }
  })
}

pickerInput.addEventListener('change', reportPickerType)
$().on('shiny:sessioninitialized', reportPickerType)

Shiny.addCustomMessageHandler('update_picker_type', updatePickerType)
