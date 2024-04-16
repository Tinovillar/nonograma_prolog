import classNames from 'classnames';
import React, { useState } from 'react';

function SwitchBtn({painting, onClick}) {
    const [isSelected, setIsSelected] = useState(false);
    return (
        // <button className={`w-36 h-20 ${painting ? "bg-black" : "bg-white"} rounded border-4 border-black`} onClick={onClick}> X
        // </button>
        <div onClick={()=>{setIsSelected(!isSelected); onClick()}} className={classNames('flex items-center w-44 h-20 cursor-pointer bg-gray-300 m-10 rounded-full',{'bg-white':isSelected})}>
            <span className={classNames('h-20 w-20 bg-black rounded-full transition-all duration-200', {"ml-24":isSelected})}></span>
        </div>
    );
}

export default SwitchBtn;