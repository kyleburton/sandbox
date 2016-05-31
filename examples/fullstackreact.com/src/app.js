import React from 'react'
import ReactDOM from 'react-dom'

const App = React.createClass({
  render: function() {
            return (<div>This is the App component</div>);
          }
});

const mountNode = document.querySelector('#root');
ReactDOM.render(<App/>, mountNode);
